.epivizrCanDaemonize <- function () {
  isTRUE(getOption("epivizrCanDaemonize"))
}

# toJSON=function(x, ...) {
#   args = list(...)
#   auto_unbox=args[["auto_unbox"]]
#   null=args[["null"]]
#   if (is.null(auto_unbox)) { args[["auto_unbox"]] = TRUE }
#   if (is.null(null)) { args[["null"]] = "null" }
#   as.character(do.call(jsonlite::toJSON, c(list(x=x), args)))
# }

json_parser <- rjson::fromJSON

#' JSON writer used by this package
#' @export
json_writer <- rjson::toJSON

