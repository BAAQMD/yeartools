test_that("new_YYYY() works", {

  # single year
  x <- new_YYYY(2005, timeline = "CY")
  expect_equal(year(x), 2005)
  expect_equal(timeline(x), "CY")

  # sequence of years
  x <- new_YYYY(2000:2010, timeline = "CY")
  expect_equal(year(x), 2000:2010)
  expect_equal(timeline(x), "CY")

  # unsorted vector of years
  x <- new_YYYY(c(2005, 2010, 2008), timeline = "CY")
  expect_equal(year(x), c(2005, 2010, 2008))
  expect_equal(timeline(x), "CY")

  # dots
  x <- new_YYYY(2003, 2004:2009, timeline = "CY")
  expect_equal(year(x), c(2003, 2004:2009))
  expect_equal(timeline(x), "CY")

  # character (all with same prefix)
  x <- new_YYYY(c("CY1990", "CY2000"))
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "CY")

})

test_that("BY() works", {
  x <- BY(2008, c(2011, 2015))
  expect_identical(timeline(x), "BY")
})

test_that("CY() works", {
  x <- CY(2011:2012)
  expect_identical(timeline(x), "CY")
})

test_that("RY() works", {
  x <- RY(2011:2012)
  expect_identical(timeline(x), "RY")
})

test_that("PY() works", {
  x <- PY(2011:2012)
  expect_identical(timeline(x), "PY")
})

test_that("can't have two prefixes", {
  expect_error(BY(RY(2011)))
  expect_error(RY(RY(2013)))
  expect_error(CY(RY(2000)))
})

test_that("new_YYYY() fails appropriately", {

  # character (inconsistent prefixes)
  expect_error(
    x <- new_YYYY(c("RY2011", "PY2011")))

})
