## epivizrServer

Methods providing WebSocket connection server and request/response message
handling for the epiviz JS application [http://epiviz.github.io](http://epiviz.github.io). These functions have been extracted from the `epivizr` [Bioconductor package](http://bioconductor.org/packages/release/bioc/html/epivizr.html) into its own package for easier use and maintenance.

It is based on the [httpuv](http://www.github.com/rstudio/httpuv) package, which provides an interface to the libuv networking library. 

## Usage 

```{r}
library(EpivizrServer)

# create the server (but it's not started yet)
server <- createServer(port=7123, verbose=FALSE)

# register a callback to evaluate when a request with given action
# is received
server$register_action("getData", function(request_data) {list(x=1,y=3)})

# start the server
server$start_server()

# send a request with callback to evaluate when successful response is received
server$send_request(list(x=2,y=5), function(response_data) {cat(response_data$x)})

# in Windows platform this is required to listen and respond to requests
server$service()

# when done, stop the server
server$stop_server()
```

