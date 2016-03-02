#'
#' Class providing an indexed array (hashtable)
#' 
#' @docType class
#' @importFrom R6 R6Class
IndexedArray <- R6Class("IndexedArray",
                        private = list(
                          nextId = 1L,
                          items = vector("list")
                        ),
                        public = list(
                          initialize = function() {
                            reg.finalizer(self,
                                          function(e) {
                                            e$empty()                 
                                          }, onexit = TRUE)
                          },
                          length = function() { base::length(private$items) },
                          append = function(item) {
                            id <- private$nextId
                            private$nextId <- private$nextId + 1L
                            private$items[[as.character(id)]] <- item
                            id
                          },
                          get = function(id) {
                            if (is.null(private$items[[as.character(id)]]))
                              return(NULL)
                            out <- private$items[[as.character(id)]]
                            private$items[[as.character(id)]] <- NULL
                            out
                          },
                          empty = function() {
                            private$items <- vector("list")
                            private$nextId <- 1L
                            invisible()
                          }
                        )
)
