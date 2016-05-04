#'
#' Class providing an indexed array (hashtable)
#' 
#' @docType class
IndexedArray <- setRefClass("IndexedArray",
                        fields = list(
                          .nextId = "integer",
                          .items = "list"
                        ),
                        methods = list(
                          initialize = function() {
                            .self$.nextId <- 1L
                            .self$.items <- vector("list")
                          },
                          length = function() { 
                            "Return number of items on array <int>"
                            base::length(.self$.items) 
                          },
                          append = function(item) {
                            "Append item to tail of array, returns id of item <int>"
                            id <- .self$.nextId
                            .self$.nextId <- .self$.nextId + 1L
                            .self$.items[[as.character(id)]] <- item
                            id
                          },
                          get = function(id) {
                            "Get item with given id<int>, returns <ANY>, returns NULL if no item with given id"
                            if (is.null(.self$.items[[as.character(id)]]))
                              return(NULL)
                            out <- .self$.items[[as.character(id)]]
                            .self$.items[[as.character(id)]] <- NULL
                            out
                          },
                          empty = function() {
                            "Remove all items from array"
                            .self$.items <- vector("list")
                            .self$.nextId <- 1L
                            invisible()
                          }
                        )
)
