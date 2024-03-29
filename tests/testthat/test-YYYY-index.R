test_that("indexing works", {
  x <- CY(2005, 2008, 2003)
  expect_true(x[1] == CY(2005))
  expect_true(x[3] == CY(2003))
  expect_true(all(x[c(3, 1, 2)] == CY(2003, 2005, 2008)))
})
