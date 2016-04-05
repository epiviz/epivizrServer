#' Class providing WebSocket connection server
#' 
#' @docType class
#' @return RC object with methods for communication with epiviz JS app
#' 
#' @details
#' The most important aspect of the API of this server are methods \code{register_action(action, callback)} and \code{send_request}. These are
#' used to interact with the epiviz JS app through the provided websocket connection. \code{register_action(action, callback)} registers
#' a callback function to be executed upon request from the epiviz JS app. When the server receives a JSON message through the websocket, it
#' checks for an \code{action} field in the received request message, and then evaluates the expression \code{callback(message_data)} where \code{message_data}
#' is obtained from the \code{data} field in the received message. A response will be sent to the epiviz app with field \code{data} populated
#' with the result of the callback. If an error occurs during evaluation of the callback function, the response will be sent with field
#' \code{success} set to \code{false}.
#' 
#' To send requests to the JS app, method \code{send_request(request_data, callback)} should be used. This is sends a request to the JS app
#' with the \code{data} field populated with argument \code{request_data}. Once a response is received (with field \code{success} equal to \code{true})
#' the expression \code{callback(response_data)} is evaluated where \code{response_data} is obtained from the \code{data} field in the received
#' response message.
#'
EpivizServer <- setRefClass("EpivizServer",
  fields = list(
    .port = "integer", 
    .try_ports = "logical", 
    .websocket = "ANY", 
    .websocket_closed = "logical", 
    .server = "ANY", 
    .interrupted = "logical", 
    .verbose = "logical", 
    .request_queue = "Queue", 
    .request_waiting = "logical", 
    .action_handlers = "list", 
    .callback_array = "IndexedArray", 
    .daemonized = "logical", 
    .start_server_fn = "function", 
    .stop_server_fn = "function" 
  ),
  methods = list(
    initialize = function(
      port=7312L, 
      try_ports=FALSE, 
      daemonized=NULL, 
      verbose=FALSE) 
    {
      .self$.port <- port
      .self$.try_ports <- try_ports
      .self$.daemonized <-  .epivizrCanDaemonize() && isTRUE(daemonized)
      .self$.start_server_fn <- if (.self$.daemonized) httpuv::startDaemonizedServer else httpuv::startServer
      .self$.stop_server_fn <- if (.self$.daemonized) httpuv::stopDaemonizedServer else httpuv::stopServer
      .self$.verbose <- verbose

      .self$.websocket <- NULL
      .self$.websocket_closed <- TRUE
      
      .self$.server <- NULL
      .self$.interrupted <- FALSE
      
      .self$.request_queue <- Queue$new()
      .self$.request_waiting <- FALSE
      
      .self$.action_handlers <- vector("list")
      
      .self$.callback_array <- IndexedArray$new()
    },
    finalize = function() { .self$stop_server() },
    .try_more_ports = function(app, minPort=7000L, maxPort=7999L) {
      success <- FALSE
      .self$.port <- minPort
      while(!success && .self$.port <= maxPort) {
        tryCatch({
          cat(".")
          .self$.server <- .self$.start_server_fn("0.0.0.0", .self$.port, app)
          success <- TRUE
        }, error=function(e) {
          .self$.port <- .self$.port + 1L
        })
      }
      invisible()
    },
    .pop_request = function() {
      if (!.self$is_socket_connected()) {
        return(invisible())
      }
      if (!.self$.request_queue$has_more()) {
        .self$.request_waiting <- FALSE
        .self$stop_service()
        return(invisible())
      }
      
      queue_entry <- .self$.request_queue$pop()
      request_data <- queue_entry$data
      callback <- queue_entry$callback
      
      request_id <- .self$.callback_array$append(callback)
      request <- list(type = "request",
                      requestId = request_id,
                      data = request_data)
      request <- json_writer(request)
      
      if (.self$.verbose) cat("SEND: ", request, "\n")
      
      # TODO: check websocket connection here
      .self$.websocket$send(request)
      .self$.request_waiting <- TRUE
      .self$service()
    },
    .handle_request = function(msg) {
      request_id <- msg$requestId
      msg_data <- msg$data
      action <- msg_data$action

      response <- list(type = "response",
                    requestId = request_id,
                    success = FALSE,
                    data = NULL)      
      
      if (.self$has_action(action)) {
        tryCatch({
          callback <- .self$.action_handlers[[action]]
          response$data <- callback(msg_data)
          response$success <- TRUE
        }, error = function(e) {
          if (.self$.verbose) {
            cat("action handler returned error:\n")
            cat(e$message, "\n")
            cat("sending unsuccessfull response\n")
          }
        })
      }
      
      response <- json_writer(response)
      if (.self$.verbose) {
        cat("SEND: ", response, "\n")
      }
      
      # TODO: check websocket is not null here
      .self$.websocket$send(response)
    },
    .handle_response = function(msg) {
      if (!isTRUE(msg$success)) {
        stop("request to JS app was unsuccessful")
      }
      
      callback <- .self$.callback_array$get(msg$requestId)
      if (!is.null(callback)) {
        callback(msg$data)
      }
      .self$.pop_request()
    },
    .message_handler = function(binary, msg) {
      if (binary) {
        msg <- rawToChar(msg)
      }
      
      if (.self$.verbose) {
        cat("RCVD: ", msg, "\n")
      }
      msg <- json_parser(msg)
      switch(msg$type,
             request = .self$.handle_request(msg),
             response = .self$.handle_response(msg))
    },
    .create_app = function(static_site_path=NULL) {
      wsHandler <- function(ws) {
        if (.self$.verbose) cat("WS opened\n")
        .self$.websocket <- ws
        .self$.websocket_closed <- FALSE
        
        .self$.websocket$onMessage(.self$.message_handler)
        .self$.websocket$onClose(function() {
          if (.self$.verbose) cat("WS closed\n")
          .self$.websocket_closed <- TRUE
          invisible()
        })
        .self$.pop_request()
        invisible()
      }
    
      if (is.null(static_site_path) || !file.exists(static_site_path)) {
        httpHandler <- .dummyTestPage
      } else {
        httpHandler <- staticHandler(static_site_path)
      }
    
      handlerMgr <- HandlerManager$new()
      handlerMgr$addHandler(httpHandler, 'static')
      handlerMgr$addWSHandler(wsHandler, 'ws')
      handlerMgr$createHttpuvApp()
    },
    show = function() {
      "Print server information to stdout"
      cat(sprintf("<EpivizServer> port: %d, %s", 
                  .self$.port, 
                  ifelse(.self$is_socket_connected(),"connected","not connected")),"\n")
      invisible()
    },
    is_closed = function() {
      "Check if server is closed, <logical>"
      is.null(.self$.server) 
    },
    is_daemonized = function() {
      "Check if server is running in background, <logical>"
      isTRUE(.self$.daemonized) 
    },
    is_socket_connected = function() { 
      "Check if there is an open websocket connection to JS app, <logical>"
      !is.null(.self$.websocket) && !.self$.websocket_closed
    },
    has_request_waiting = function() { 
      "Check if there is a sent request waiting for a response from JS app, <logical>"
      .self$.request_waiting 
    },
    stop_server = function() { 
      "Stop the underlying httpuv server"
      .self$.interrupted <- TRUE
      if (.self$.websocket_closed) {
        .self$.websocket <- NULL
      }
      
      if (!.self$is_closed()) {
        .self$.stop_server_fn(.self$.server)
      }
      .self$.server <- NULL
      .self$.interrupted <- FALSE
      invisible()
    },
    start_server = function(static_site_path=NULL) {
      "Start the underlying httpuv server, daemonized if applicable"
      app <- .self$.create_app(static_site_path)
      
      tryCatch({
        .self$.server <- .self$.start_server_fn("0.0.0.0", .self$.port, app)
      }, error = function(e) {
        if (!.self$.try_ports)
          stop(sprintf("Error starting epivizServer, likely because port %d is in use.\nTry a different port number or setting try_ports=TRUE (see ?startEpiviz).", .self$.port))
        .self$.try_more_ports(app)
      })
      invisible()
    },
    register_action = function(action, callback) {
      "Register a callback<function> to evaluate when epiviz JS sends a request for given action<character>. (See Details)"
      if (!is.character(action)) {
        stop("action must be a string")
      }
      if (!is.null(.self$.action_handlers[[action]])) {
        stop(sprintf("action %s is already registered", action))
      }
      .self$.action_handlers[[action]] <- callback
      invisible()
    },
    unregister_action = function(action) {
      "Unregister a callback function for given action<character> (if registered). (See Details)"
      if (!is.null(.self$.action_handlers[[action]])) {
        .self$.action_handlers[[action]] <- NULL
      }
      invisible()
    },
    has_action = function(action) { 
      "Check if a callback is registered for given action<character>, <logical>. (See Details)"
      !is.null(.self$.action_handlers[[action]]) 
    },
    send_request = function(request_data, callback) {
      "Send request to epiviz JS app with given request_data<list>, and evaluate callback<function> when response arrives. (See Details)"
      .self$.request_queue$push(list(data=request_data, callback=callback))
      if (!.self$.request_waiting)
        .self$.pop_request()
      invisible()
    },
    service=function(nonInteractive=FALSE) {
      "Listen to requests from server. Only has effect when non-daemonized"
      if (.self$is_closed()) {
        stop("Can't listen, socket is closed")
      }

      if (.self$is_daemonized())
        return(invisible(TRUE))

      if (nonInteractive) {
        # run service loop once
        httpuv::service()
        return(invisible(TRUE))
      }

      .self$.interrupted <- FALSE
      while(!.self$.interrupted) {
        httpuv::service()
        Sys.sleep(0.001)
      }
      invisible()
    },
    stop_service=function() {
      "Stop listenning to requests from server. Only has effect when non-daemonized."
      .self$.interrupted <- TRUE
      invisible()
    },
    run_server=function(...) {
      "Run server in blocking mode"
      .self$start_server(...)
      on.exit(.self$stop_server())
      .self$service()
    }
  )
)
