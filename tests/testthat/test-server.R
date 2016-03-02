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
