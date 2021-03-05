
test_that("is.string", {
  expect_true(is.string("foo"))
  expect_true(is.string(""))
  expect_false(is.string(NA_character_))
  expect_false(is.string(letters))
  expect_false(is.string(1:10))
  expect_false(is.string(1))
})

test_that("with_something", {
  in_dir <- with_something(setwd)
  expect_equal(
    normalizePath(in_dir(tempdir(), getwd())),
    normalizePath(tempdir())
  )
})

test_that("in_dir", {
  expect_equal(
    normalizePath(in_dir(tempdir(), getwd())),
    normalizePath(tempdir())
  )
})

test_that("%||%", {
  expect_equal("a" %||% NULL, "a")
  expect_equal(NULL %||% "a", "a")
})

test_that("os_type", {
  expect_equal(os_type(), .Platform$OS.type)
})
