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
    pop_request = function() {
      if (!self$socket_connected()) {
        return(invisible())
      }
      request <- private$request_queue$pop()
      if (is.null(request)) {
        private$request_waiting <- FALSE
        public$stop_service()
        return(invisible())
      }
      request <- json_writer(request)
      if (verbose) epivizrMsg("SEND: ", request)
      private$websocket$send(request)
      private$request_waiting <- TRUE
      self$service()
    },
    handle_request = function(msg) {
      request_id <- msg$requestId
      msg_data <- msg$data
      action <- msg_data$action

      response <- list(type = "response",
                    requestId = request_id,
                    success = FALSE,
                    data = NULL)      
      
      if (self$has_action(action)) {
        tryCatch({
          callback <- private$action_handlers[[action]]
          response$data <- callback(msg_data)
          response$success <- TRUE
        })
      }
      
      response <- json_writer(response)
      if (private$verbose) {
        cat("SEND: ", response, "\n")
      }
      
      # TODO: check websocket is not null here
      private$websocket$send(response)
    },
    handle_response = function(msg) {
      # TODO: check response success
      callback <- private$callbackArray$get(msg$requestId)
      if (!is.null(callback)) {
        callback(msg$data)
      }
      self$pop_request()
    },
    message_handler = function(binary, msg) {
      if (binary) {
        msg <- rawToChar(msg)
      }
      
      if (private$verbose) {
        cat("RCVD: ", msg, "\n")
      }
      msg <- json_parser(msg)
      switch(msg$type,
             request = private$handle_request(msg),
             response = private$handle_response(msg))
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
    has_request_waiting = function() { private$request_waiting },
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
    send_request = function(request) {
      private$request_queue$push(request)
      if (!private$request_waiting)
        private$pop_request()
      invisible()
    },
    service=function(nonInteractive=FALSE) {
      if (self$is_closed()) {
        stop("Can't listen, socket is closed")
      }

      if (self$is_daemonized())
        return(invisible(TRUE))

      if (nonInteractive) {
        # run service loop once
        httpuv::service()
        return(invisible(TRUE))
      }

      private$interrupted <- FALSE
      while(!private$interrupted) {
        httpuv::service()
        Sys.sleep(0.001)
      }
      invisible()
    },
    stop_service=function() {
      private$interrupted <- TRUE
      invisible()
    },
    run_server=function(...) {
      self$start_server(...)
      on.exit(self$stop_server())
      self$service()
    }
  )
)
