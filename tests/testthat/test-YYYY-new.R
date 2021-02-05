test_that("constructor works", {

  # single year
  x <- new_YYYY(2005, timeline = "CY")
  expect_is(x, "YYYY")

  # sequence of years
  x <- new_YYYY(2000:2010, timeline = "CY")
  expect_is(x, "YYYY")

  # unsorted vector of years
  x <- new_YYYY(c(2005, 2010, 2008), timeline = "CY")
  expect_is(x, "YYYY")

  # dots
  x <- new_YYYY(2003, 2004:2009, timeline = "CY")
  expect_is(x, "YYYY")

})
