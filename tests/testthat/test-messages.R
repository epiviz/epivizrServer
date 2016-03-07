context("messages")

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

test_that("handle request works", {
  skip("skip this test for now")
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
  
  lastMessage <- ""
  server$register_action("update", function(msg_data) {
    lastMessage <<- msg_data$message
  })
  server$start_server()

  expect_false(server$is_closed())

  # use selenium to write a message on page
  # check lastMessage is updated
  inputEl <- remDr$findElement(using="id", "input")
  message_text <- "This is a message from JS"
  inputEl$sendKeysToElement(list(message_text, key="enter"))
  wait_until(!server$requestWaiting)
  
  # check it made it to response list
  outputEl <- remDr$findElement(using="id", "output")  
  responseEl <- outputEl$findChildElement(using="css", "pre")
  response_text <- responseEl$getElementText()[[1]]
  expect_equal(response_text, message_text)
  
  # check message made it here
  expect_equal(lastMessage, message_text)
  
  remDr$close()
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

