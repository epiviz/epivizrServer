context("IndexedArray")

test_that("creation works", {
  array <- IndexedArray$new()
  expect_is(array, "IndexedArray")
  expect_equal(array$length(), 0)
})

test_that("append and get works", {
  array <- IndexedArray$new()
  id1 <- array$append(1)
  expect_equal(id1, 1L)
  
  id2 <- array$append(2)
  expect_equal(id2, 2L)
  expect_equal(array$length(), 2)
  
  res2 <- array$get(2L)
  expect_equal(array$length(), 1)
  expect_equal(res2, 2)
  
  res1 <- array$get(1L)
  expect_equal(array$length(), 0)
  expect_equal(res1, 1)
})

test_that("empty works", {
  array <- IndexedArray$new()
  array$append(1)
  array$append(2)
  array$empty()
  
  expect_equal(array$length(), 0)
  id1 <- array$append(1)
  expect_equal(id1, 1L)
})