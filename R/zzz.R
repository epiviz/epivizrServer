.onLoad <- function(libname, pkgname) {
  tryCatch({
    if (is.function(httpuv::startDaemonizedServer))
      options(epivizrCanDaemonize = TRUE)
  }, error=function(e) {
    options(epivizCanDaemonize = FALSE)
  })
}
