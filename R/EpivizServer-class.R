#' Class providing WebSocket connection server
#' 
#' @docType class
#' @importFrom R6 R6Class
EpivizServer <- R6Class("EpivizServer",
  private = list(
    port = 7312L,
    try_ports = FALSE,
    websocket = NULL,
    websocket_closed = FALSE,
    server = NULL,
    interrupted = FALSE,
    verbose = FALSE,
    request_queue = NULL,
    request_waiting = FALSE,
    action_handlers = NULL,
    callback_array = NULL,
    daemonized = FALSE,
    start_server_fn = NULL,
    stop_server_fn = NULL,
    try_more_ports = function(app, minPort=7000L, maxPort=7999L) {
      success <- FALSE
      private$port <- minPort
      while(!success && private$port <= maxPort) {
        tryCatch({
          cat(".")
          private$server <- private$start_server_fn("0.0.0.0", private$port, app)
          success <- TRUE
        }, error=function(e) {
          private$port <- private$port + 1L
        })
      }
      invisible()
    },
    message_handler = function(binary, msg) {
      if (binary) {
        msg <- rawToChar(msg)
      }
      
      if (private$verbose) {
        cat("RCVD: \n", msg)
      }
      msg <- json_parser(msg)
      invisible()
    },
    create_app = function() {
      wsHandler <- function(ws) {
        if (private$verbose) cat("WS opened\n")
        private$websocket <- ws
        private$websocket_closed <- FALSE
        
        private$websocket$onMessage(private$message_handler)
        private$websocket$onClose(function() {
          if (private$verbose) cat("WS closed\n")
          private$websocket_closed <- TRUE
          invisible()
        })
        #self$pop_request()
        invisible()
      }
    
      httpHandler <- .dummyTestPage
    
      handlerMgr <- HandlerManager$new()
      handlerMgr$addHandler(httpHandler, 'static')
      handlerMgr$addWSHandler(wsHandler, 'ws')
      handlerMgr$createHttpuvApp()
    }
  ),
  public = list(
    initialize = function(
        port=7312L, 
        try_ports=FALSE, 
        daemonized=NULL, 
        verbose=FALSE) {
      private$port <- port
      private$try_ports <- try_ports
      private$daemonized <-  .epivizrCanDaemonize() && isTRUE(daemonized)
      private$start_server_fn <- if (private$daemonized) httpuv::startDaemonizedServer else httpuv::startServer
      private$stop_server_fn <- if (private$daemonized) httpuv::stopDaemonizedServer else httpuv::stopServer
      private$verbose <- verbose
      
      private$request_queue <- Queue$new()
      private$request_waiting <- FALSE
      
      private$action_handlers <- vector("list")
      
      private$callback_array <- IndexedArray$new()
      
      reg.finalizer(self,
                    function(e) {
                      e$stop_server()
                    }, onexit=TRUE)
    },
    show=function() {
      cat(sprintf("<EpivizServer> port: %d, %s", private$port, ifelse(self$is_socket_connected(),"connected","not connected")),"\n")
      invisible()
    },
    is_closed = function() { is.null(private$server) },
    is_daemonized = function() { isTRUE(private$daemonized) },
    is_socket_connected = function() { !is.null(private$websocket) && !private$websocket_closed},
    stop_server = function() { 
      private$interrupted <- TRUE
      if (private$websocket_closed) {
        private$websocket <- NULL
      }
      
      if (!self$is_closed()) {
        private$stop_server_fn(private$server)
      }
      private$server <- NULL
      private$interrupted <- FALSE
      invisible()
    },
    start_server = function() {
      app <- private$create_app()
      
      tryCatch({
        private$server <- private$start_server_fn("0.0.0.0", private$port, app)
      }, error = function(e) {
        if (!private$try_ports)
          stop(sprintf("Error starting epivizServer, likely because port %d is in use.\nTry a different port number or setting try_ports=TRUE (see ?startEpiviz).", private$port))
        private$try_more_ports(app)
      })
      invisible()
    },
    register_action = function(action, callback) {
      if (!is.character(action)) {
        stop("action must be a character string")
      }
      if (!is.null(private$action_handlers[[action]])) {
        stop(sprintf("action %s is already registered", action))
      }
      private$action_handlers[[action]] <- callback
      invisible()
    },
    has_action = function(action) { !is.null(private$action_handlers[[action]]) },
    handle = function(action, msg_data) { 
      if (!self$has_action(action)) {
        stop("action %s is not registered", action)
      }
      callback <- private$action_handlers[[action]]
      callback(msg_data)
    },
    service=function(nonInteractive=FALSE) {
      if (isClosed()) {
        stop("Can't listen, socket is closed")
      }

      if (daemonized)
        return(invisible(TRUE))

      if (nonInteractive) {
        # run service loop once
        httpuv::service()
        return(invisible(TRUE))
      }


      interrupted <<- FALSE
      while(!interrupted) {
        httpuv::service()
        Sys.sleep(0.001)
      }
      invisible(TRUE)
    },
    stopService=function() {
      interrupted <<- TRUE
      invisible()
    },
    runServer=function(...) {
      startServer(...)
      on.exit(stopServer())
      service()
    },
    bindManager=function(mgr) {
      msgCallback <<- function(binary, msg) {
        
        if (msg$type == "request") {
          out=list(type="response",
            requestId=msg$requestId)
          msgData=msg$data
          action=msgData$action
          # request handling
# defined here: http://epiviz.github.io/dataprovider-plugins.html

          out$data=NULL
          if (action == "getAllData") {
              out$data <- list(msg=msgData$chr)
          } else {
              out$data <- mgr$handle(action, msgData)
          }
          response <- json_writer(out)
          if (verbose) {
            epivizrMsg("SEND: ", response)
          }
          websocket$send(response)
        } else if (msg$type == "response") {
          # TODO: check response success
          callback = mgr$callbackArray$get(msg$requestId)
          if (!is.null(callback)) {
            callback(msg$data)
          }
          popRequest()
        }
      }
      invisible()
    },
    sendRequest=function(request) {
      requestQueue$push(request)
      if (!requestWaiting)
        popRequest()
      invisible()
    },
    popRequest=function() {
      if (!socketConnected) {
        return(invisible())
      }
      request <- requestQueue$pop()
      if (is.null(request)) {
        requestWaiting <<- FALSE
        stopService()
        return(invisible())
      }
      request <- toJSON(request)
      if (verbose) epivizrMsg("SEND: ", request)
      websocket$send(request)
      requestWaiting <<- TRUE
      service()
    },
    emptyRequestQueue=function() {
      requestQueue$empty()
      inivisible()
    }
  )
)
