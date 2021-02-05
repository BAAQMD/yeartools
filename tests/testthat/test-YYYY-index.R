test_that("indexing works", {
  x <- new_YYYY(2005, 2008, 2003, timeline = "CY")
  expect_equal(x[1], CY(2005))
  expect_equal(x[3], CY(2003))
  expect_equal(x[c(3, 1, 2)], CY(2003, 2005, 2008))
})
