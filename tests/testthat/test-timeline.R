test_that("timeline() works", {

  x <- CY(2010:2020)
  expect_equal(timeline(x), "CY")

  x <- c("CY2001", "CY2005")
  expect_equal(timeline(x), "CY")

})
