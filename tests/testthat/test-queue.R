context("Queue")

test_that("construction works", {
  queue <- Queue$new()
  expect_is(queue, "Queue")
  expect_equal(queue$length(), 0)
})

test_that("push and pop work", {
  queue <- Queue$new()
  queue$push(1)
  queue$push(2)
  
  expect_equal(queue$length(), 2)
  expect_true(queue$has_more())
  expect_equal(queue$pop(), 1)
  expect_true(queue$has_more())
  
  expect_equal(queue$pop(), 2)
  expect_false(queue$has_more())
})

test_that("empty works", {
  queue <- Queue$new()
  queue$push(1)
  queue$push(2)
  
  expect_true(queue$has_more())
  queue$empty()
  expect_false(queue$has_more())
})