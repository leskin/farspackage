context("make_filename")

## TODO: Add more tests

test_that("make_filename works for 2013", {
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})
