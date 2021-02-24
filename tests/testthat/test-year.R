test_that("year() works", {

  x <- CY(2011:2012)

  expect_identical(
    year(x),
    c(2011L, 2012L))

})
