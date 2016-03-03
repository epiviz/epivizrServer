context("messages")

test_that("registering action works", {
  server <- EpivizServer$new(daemonized = FALSE)
  server$register_action("update", function(msg_data) {
    last_message <<- msg_data$message
  })
  expect_true(server$has_action("update"))
  
  last_message <- ""
  server$handle("update", list(message="hello there"))
  expect_equal(last_message, "hello there")
})

.canPhantomTest <- function() {
  if(.Platform$OS.type == "windows") { return(FALSE) }
  if(!require(RSelenium)) { return(FALSE) }
  if(Sys.which("phantomjs") == "") { return(FALSE) }
  if(!getOption("epivizrCanDaemonize")) { return(FALSE) }
  TRUE
}

remDr <- NULL
pJS <- NULL

.startRemoteDriver <- function() {
  if(!.canPhantomTest()) {
    stop("can't do headless testing here")
  }
  pJS <<- phantom()
  Sys.sleep(2)
  
  remDr <<- remoteDriver(browserName = 'phantomjs')
  res <- remDr$open()
  invisible()
}

.navigateRemoteDriver <- function(url) {
  parallel::mcparallel(remDr$navigate(url), detached=TRUE)
  Sys.sleep(2)
}

.stopPhantomJS <- function() {
  pJS$stop()
}

test_that("socket connection works", {
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  .navigateRemoteDriver("http://127.0.0.1:7123")
  wait_until(server$is_socket_connected())
  
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
  
  remDr$close()
})

test_that("socket messaging works", {
  skip("skip this test for now")
  server <- EpivizServer$new(port=7123L, daemonized=TRUE)
  if (!server$is_daemonized())
    skip("This test only works for daemonized servers")
  
  lastMessage <- ""
  server$register_action("update", function(msg_data) {
    lastMessage <<- msg_data$message
  })
  server$start_server()
  
  # change to RSelenium here
  browseURL("http://localhost:7123/")

  tryCatch(server$service(), interrupt=function(int) invisible())
  wait_until(server$socketConnected)
  
  expect_false(server$is_closed())

  request <- mgr$makeRequest("this msg")
  server$sendRequest(request)

  wait_until(!server$requestWaiting)
  server$stop_server()
  expect_true(server$is_closed())
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

