#' Create a new EpivizServer object
#'
#' @export
#'  
#' @param port Port to which server will listen to <int>
#' @param try_ports Try various ports until an open port is found <logical>
#' @param daemonized Run in background using httpuv's daemonized libuv server <logical>
#' @param verbose Print verbose output <logical>
#' @return an 'EpivizServer' object
createServer <- function(port=7123L,
                         try_ports=FALSE,
                         daemonized=NULL,
                         verbose=FALSE) {
  EpivizServer$new(port = as.integer(port), 
                   try_ports = try_ports,
                   daemonized = daemonized,
                   verbose = verbose)
}