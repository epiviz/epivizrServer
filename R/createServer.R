#' Create a new EpivizServer object
#'
#' @export
#'  
#' @param port (int) port to which server will listen to.
#' @param static_site_path (character) path to serve static html files.
#' @param try_ports (logical) try various ports until an open port is found.
#' @param daemonized (logical) run in background using httpuv's daemonized libuv server.
#' @param verbose (logical) print verbose output.
#' @param non_interactive (logical) run in non-interactive mode. For development purposes only.
#' 
#' @return an \code{\link{EpivizServer}} object
#' 
#' @seealso \code{\link{EpivizServer}} for the class of objects returned
#' @examples
#' server <- createServer(port=7123,
#'                        verbose=TRUE
#'                        )
#' @import tools                        
createServer <- function(port=7123L,
                         static_site_path="",
                         try_ports=FALSE,
                         daemonized=NULL,
                         verbose=FALSE,
                         non_interactive=FALSE) {
  static_site_path <- as.character(static_site_path)
  if (nchar(static_site_path) > 0) {
    static_site_path <- tools::file_path_as_absolute(static_site_path)
  }
  EpivizServer$new(port = as.integer(port),
                   static_site_path = static_site_path,
                   try_ports = try_ports,
                   daemonized = daemonized,
                   verbose = verbose,
                   non_interactive = non_interactive)
}