#' Class providing a queue data structure
#' 
#' @docType class
#' @importFrom R6 R6Class
Queue <- R6Class("Queue",
                 private = list(
                   items = vector("list")
                 ),
                 public = list(
                  initialize = function() {
                    reg.finalizer(self,
                      function(e) {
                        e$empty()
                      }, onexit=TRUE)
                  },
                  length = function() { base::length(private$items) },
                  has_more = function() { self$length() > 0 },       
                  push = function(item) {
                    n <- self$length()
                    private$items[[n+1]] <- item
                    invisible()
                  },
                  pop = function() {
                    if (!self$has_more()) return(NULL)
                    
                    out <- private$items[[1]]
                    private$items[[1]] <- NULL
                    out
                  },
                  empty = function() {
                    private$items <- vector("list")
                  }
                 )
)
