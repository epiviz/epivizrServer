context("messages")

.canPhantomTest <- function() {
  if(Sys.which("phantomjs") == "") { return(FALSE) }
  if(!getOption("epivizrCanDaemonize")) { return(FALSE) }
  TRUE
}

remDr <- NULL
pJS <- NULL

.startRemoteDriver <- function() {
  if(!require(RSelenium)) {
    stop("can't run this test here")
  }
  
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
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server(static_site_path=".")
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  .navigateRemoteDriver("http://127.0.0.1:7123")
  wait_until(server$is_socket_connected())
  
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
  
  remDr$close()
})

test_that("handle request works", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server(static_site_path=".")
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  .navigateRemoteDriver("http://127.0.0.1:7123")
  wait_until(server$is_socket_connected())
  
  lastMessage <- ""
  server$register_action("update", function(msg_data) {
    lastMessage <<- msg_data$message
    list(message="This is the response from R")
  })
  
  expect_false(server$is_closed())

  # use selenium to write a message on page
  # check lastMessage is updated
  inputEl <- remDr$findElement(using="id", "input")
  message_text <- "This is a message from JS"
  inputEl$sendKeysToElement(list(message_text, key="enter"))
  wait_until(!server$has_request_waiting())
  
  Sys.sleep(2)
  # check it made it to response list
  outputEl <- remDr$findElement(using="id", "response_output")  
  responseEl <- outputEl$findChildElement(using="css", "pre")
  response_text <- responseEl$getElementText()[[1]]
  expect_equal(response_text, "This is the response from R")
  
  # check message made it here
  expect_equal(lastMessage, message_text)
  
  remDr$close()
})

test_that("send request works", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server(static_site_path=".")
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  .navigateRemoteDriver("http://127.0.0.1:7123")
  wait_until(server$is_socket_connected())
  
  lastMessage <- ""
  server$register_action("update", function(msg_data) {
    lastMessage <<- msg_data$message
    list(message="This is the response from R")
  })

  expect_false(server$is_closed())

  lastMessage <<- ""
  request_text_from_R <- "This is a message from R"
  server$send_request(list(message=request_text_from_R), 
                      function(response_data) {
                        cat("This is the callback function\n")
                        print(response_data)
                        lastMessage <<- response_data$message
                      })
  Sys.sleep(2)
  outputEl <- remDr$findElement(using="id", "request_output")
  requestEl <- outputEl$findChildElement(using="css", "pre")
  request_text_in_JS <- requestEl$getElementText()[[1]]
  expect_equal(request_text_in_JS, request_text_from_R)
  
  expect_equal(lastMessage, "this is the response from JS")
  remDr$close()
})

