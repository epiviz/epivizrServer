context("server")

test_that("non-daemonized constructor creates a proper object", {
  server <- EpivizServer$new(port=7123L, daemonized=FALSE, verbose=TRUE)
  expect_is(server, "EpivizServer")
  expect_true(server$is_closed())
  expect_false(server$is_daemonized())
})

test_that("daemonized constructor creates a proper object", {
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  expect_is(server, "EpivizServer")
  expect_true(server$is_closed())
  expect_true(server$is_daemonized())
})

test_that("non-daemonized start_server and stop_server work appropriately", {
  server <- EpivizServer$new(port=7123L, daemonized=FALSE, verbose=TRUE)
  expect_true(server$is_closed())
  
  server$start_server()
  expect_false(server$is_closed())
  expect_false(server$is_daemonized())
  
  server$stop_server()
  expect_true(server$is_closed())
})

test_that("daemonized startServer and stopServer work appropriately", {
  server <- EpivizServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  expect_true(server$is_closed())
  
  server$start_server()
  expect_false(server$is_closed())
  expect_true(server$is_daemonized())
  server$stop_server()
  expect_true(server$is_closed())
})

test_that("using a port again is an error", {
  server <- EpivizServer$new(port=7123L, daemonized=FALSE)
  server$start_server()

  server2 <- EpivizServer$new(port=7123L, daemonized=FALSE)
  expect_error(server2$start_server())
  
  server$stop_server()
  expect_true(server$is_closed())
})

test_that("try_ports works", {
  server <- EpivizServer$new(port=7123L, daemonized=FALSE)
  server$start_server()
  
  expect_false(server$is_closed())
  
  server2 <- EpivizServer$new(port=7123L, daemonized=FALSE, try_ports=TRUE)
  server2$start_server()
  expect_false(server2$is_closed())
  
  server$stop_server()
  server2$stop_server()
  expect_true(server$is_closed())
  expect_true(server2$is_closed())
})


