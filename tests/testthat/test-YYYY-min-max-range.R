x <- new_YYYY(c(2010, 2005, 2019), timeline = "CY")

test_that("min() works", {
  expect_equal(min(x), CY(2005))
})

test_that("max() works", {
  expect_equal(max(x), CY(2019))
})

test_that("range() works", {
  expect_equal(range(x), CY(2005, 2019))
})
