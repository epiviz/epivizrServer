#' Create a new EpivizServer object
#'
#' @export
#'  
#' @param port Port to which server will listen to <int>
#' @param static_site_path Path to serve static html files <char>
#' @param try_ports Try various ports until an open port is found <logical>
#' @param daemonized Run in background using httpuv's daemonized libuv server <logical>
#' @param verbose Print verbose output <logical>
#' @return an \code{\link{EpivizServer}} object
#' 
#' @seealso \code{\link{EpivizServer}} for the class of objects returned
#' @examples
#' server <- createServer(port=7123,
#'                        verbose=TRUE
#'                        )
createServer <- function(port=7123L,
                         static_site_path="",
                         try_ports=FALSE,
                         daemonized=NULL,
                         verbose=FALSE) {
  EpivizServer$new(port = as.integer(port),
                   static_site_path = as.character(static_site_path),
                   try_ports = try_ports,
                   daemonized = daemonized,
                   verbose = verbose)
}