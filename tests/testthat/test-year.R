test_that("year() works", {

  x <- CY(2011:2012)

  expect_identical(
    year(x),
    as.character(c(2011L, 2012L)))

})
