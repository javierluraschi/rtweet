
context("lookup_coords")

test_that("lookup_coords returns coords data", {
  skip_on_cran()

  x <- lookup_coords("usa")
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true("box" %in% names(x))

  x <- lookup_coords("New York, NY")
  expect_equal(is.list(x), TRUE)
  expect_named(x)
  expect_true("box" %in% names(x))
})

