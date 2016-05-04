#' Class providing a queue data structure
#' 
#' @docType class
Queue <- setRefClass("Queue",
                 fields = list(.items = "list"),
                 methods = list(
                  initialize = function() {
                    .self$.items <- vector("list")
                  },
                  length = function() { 
                    "Return the number of items in queue <int>"
                    base::length(.self$.items) 
                  },
                  has_more = function() { 
                    "Return TRUE if there are more items in queue <logical>"
                    .self$length() > 0 
                  },       
                  push = function(item) {
                    "Push <item> onto queue"
                    n <- .self$length()
                    .self$.items[[n+1]] <- item
                    invisible()
                  },
                  pop = function() {
                    "Pop next item from queue (returns NULL if queue is empty)"
                    if (!.self$has_more()) return(NULL)
                    
                    out <- .self$.items[[1]]
                    .self$.items[[1]] <- NULL
                    out
                  },
                  empty = function() {
                    "Remove all items from queue"
                    .self$.items <- vector("list")
                    invisible()
                  }
                 )
)
