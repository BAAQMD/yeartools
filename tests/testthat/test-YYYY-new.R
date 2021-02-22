test_that("new_YYYY() works", {

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

  # character (all with same prefix)
  x <- new_YYYY(c("CY1990", "CY2000"))
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "CY")

})

test_that("BY() works", {
  x <- BY(2008, c(2011, 2015))
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "BY")
})

test_that("CY() works", {
  x <- CY(2011:2012)
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "CY")
})

test_that("RY() works", {
  x <- RY(2011:2012)
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "RY")
})

test_that("PY() works", {
  x <- PY(2011:2012)
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "PY")
})

test_that("new_YYYY() fails appropriately", {

  # character (inconsistent prefixes)
  expect_error(
    x <- new_YYYY(c("RY2011", "PY2011")))

})
