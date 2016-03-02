context("server")

constrFunction <- function(...) createServer(daemonized=getOption("epivizrTestDaemonized"),
                                             verbose=TRUE, ...)


mgr <- new.env()
mgr$lastMessage <- ""
mgr$callbackArray <- epivizr:::IndexedArray$new()
mgr$verbose <- TRUE

mgr$makeRequest <- function(msg) {
                           callback=function(data) {
                             mgr$lastMessage <<- data$msg
                             epivizrMsg("Response received")
                           }
                           requestId <- mgr$callbackArray$append(callback)
                           list(type="request",
                                requestId=requestId,
                                data=list(action="writeMsg",
                                  msg=msg))
                         }
mgr$getSeqInfos <- function(msg) return(NULL)
mgr$getMeasurements <- function(msg) return(NULL)
mgr$getRows <- function(msg) return(NULL)
mgr$getValues <- function(msg) return(NULL)


test_that("non-daemonized constructor creates a proper object", {
  server <- createServer(port=7123L, daemonized=FALSE, verbose=TRUE)
  expect_is(server, "EpivizServer")
  expect_true(server$isClosed())
  expect_equal(server$daemonized, FALSE)
})

test_that("daemonized constructor creates a proper object", {
  server <- createServer(port=7123L, daemonized=TRUE, verbose=TRUE)
  expect_is(server, "EpivizServer")
  expect_true(server$isClosed())
  expect_equal(server$daemonized, TRUE)
})

test_that("non-daemonized startServer and stopServer work appropriately", {
  skip("skip this test for now")
  server <- startServer(port=7123L, daemonized=FALSE, verbose=TRUE)
  expect_true(server$isClosed())
  
  server$startServer()
  expect_false(server$isClosed())
  expect_equal(server$daemonized, FALSE)
  server$stopServer()
  expect_true(server$isClosed())
})

test_that("daemonized startServer and stopServer work appropriately", {
  skip("skip this test for now")
  server <- daemonized(port=7123L, daemonized=TRUE, verbose=TRUE)
  expect_true(server$isClosed())
  
  server$startServer()
  expect_false(server$isClosed())
  expect_equal(server$daemonized, TRUE)
  server$stopServer()
  expect_true(server$isClosed())
})

test_that("socket messaging works", {
  skip("Messaging not available yet")
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()

  # change to RSelenium here
  browseURL("http://localhost:7123/")

  tryCatch(server$service(), interrupt=function(int) invisible())
  wait_until(server$socketConnected)
  
  expect_false(server$isClosed())

  request <- mgr$makeRequest("this msg")
  server$sendRequest(request)

  wait_until(!server$requestWaiting)
  server$stopServer()
  expect_true(server$isClosed())
})
  

test_that("new error message is displayed", {
  skip("skip this test for now")
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()
#  tryCatch(server$service(), interrupt=function(int) invisible())
 # expect_false(server$isClosed())

  server2 <- constrFunction(port=7123L)
  expect_error(tryCatch(server2$startServer(),error=function(e){print(e); stop(e)}))

  server$stopServer()
  expect_true(server$isClosed())
})

test_that("tryPorts works", {
  skip("skip this test for now")
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()

#  browseURL(sprintf("http://localhost:7312/")
 ## tryCatch(server$service(), interrupt=function(int) invisible())
  #wait_until(substitute(server$socketConnected))
  
  expect_false(server$isClosed())

  server2 <- constrFunction(port=7123L, tryPorts=TRUE)
  server2$bindManager(mgr)
  server2$startServer()
  #tryCatch(server2$service(), interrupt=function(int) invisible())
 # wait_until(substitute(server$socketConnected))
  
  expect_false(server2$isClosed())

  if (!server2$standalone) {
    browseURL(sprintf("http://localhost:%d/", server2$port))
  } else {
    browseURL(sprintf("http://localhost:%d/index-standalone.html", server2$port))
  }
  tryCatch(server2$service(), interrupt=function(int) invisible())
  wait_until(server2$socketConnected)
  
  server$stopServer()

  server2$sendRequest(mgr$makeRequest("this other msg"))
  server2$stopServer()
  expect_true(server$isClosed())
  expect_true(server2$isClosed())
})

