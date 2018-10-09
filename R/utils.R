.epivizrCanDaemonize <- function () {
  isTRUE(getOption("epivizrCanDaemonize"))
}

#' JSON parser used by this package
#'
#' Currently this just renames \code{\link{fromJSON}} in the \code{rjson} package.
#' @importFrom rjson fromJSON
#' @export
#'
#' @param json_str json string to parse
#' @param file file to read json_Str from
#' @param method method used to parse json
#' @param unexpected.escape handling escape characters, one of error, skip, keep
#' @param simplify if TRUE, convert json-encoded lists to vectors
#' @return a JSON object
#'
#' @seealso \code{\link{fromJSON}}
json_parser <- rjson::fromJSON

#' JSON writer used by this package
#'
#' Currently this just renames \code{\link{toJSON}} in the \code{rjson} package.
#' @importFrom rjson toJSON
#' @export
#'
#' @param x object to write to json
#' @param indent integer specifying how much indentation to use when formatting the JSON object; if 0, no pretty-formatting is used
#' @param method method used to write json
#' @return a string with JSON encoding of object
#'
#' @seealso \code{\link{toJSON}}
json_writer <- rjson::toJSON

