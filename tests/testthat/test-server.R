context("server")

test_that("non-daemonized constructor creates a proper object", {
  server <- EpivizrServer$new(port=7123L, daemonized=FALSE, verbose=TRUE)
  expect_is(server, "EpivizrServer")
  expect_true(server$is_closed())
  expect_false(server$is_daemonized())
})

test_that("daemonized constructor creates a proper object", {
  server <- EpivizrServer$new(port=7123L, daemonized=TRUE, verbose=TRUE)
  expect_is(server, "EpivizrServer")
  expect_true(server$is_closed())
  expect_true(server$is_daemonized())
})
