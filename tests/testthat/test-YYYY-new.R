test_that("YYYY() works", {

  # single year
  x <- YYYY(2005, prefix = "CY")
  expect_identical(year(x), "2005")
  expect_identical(parse_year(x), 2005L)
  expect_identical(timeline(x), "CY")

  # sequence of years
  x <- YYYY(2000:2010, prefix = "CY")
  expect_identical(parse_year(x), 2000:2010L)
  expect_identical(timeline(x), "CY")

  # unsorted vector of years
  x <- YYYY(c(2005, 2010, 2008), prefix = "CY")
  expect_identical(parse_year(x), c(2005L, 2010L, 2008L))
  expect_identical(timeline(x), "CY")

  # dots
  x <- YYYY(2003, 2004:2009, prefix = "CY")
  expect_identical(parse_year(x), c(2003L, 2004:2009L))
  expect_identical(timeline(x), "CY")

  # character (all with same prefix)
  x <- YYYY(c("CY1990", "CY2000"))
  expect_is(x, "YYYY")
  expect_identical(timeline(x), "CY")

})

test_that("BY(2008, c(2011, 2015)) works", {
  x <- BY(2008, c(2011, 2015))
  expect_identical(timeline(x), "BY")
  expect_equal(length(x), 3)
  expect_equal(format(x), c("BY2008", "BY2011", "BY2015"))
})

test_that("CY(2011:2012) works", {
  x <- CY(2011:2012)
  expect_identical(timeline(x), "CY")
  expect_equal(length(x), 2)
})

test_that("RY() works", {
  x <- RY()
  expect_identical(timeline(x), "RY")
  expect_equal(length(x), 0)
})

test_that("PY(2011:2012) works", {
  x <- PY(2011:2012)
  expect_identical(timeline(x), "PY")
  expect_equal(length(x), 2)
  expect_identical(parse_year(x), c(2011L, 2012L))
  expect_identical(elide_year(x), c("2011", "2012"))
})

test_that("can't have two different prefixes", {
  expect_error(BY(RY(2011)))
  expect_error(CY(RY(2000)))
})

test_that("YYYY() fails appropriately", {
  expect_error(YYYY(c("RY2011", "PY2011"))) # can't have more than one distinct prefix
})

test_that("new_YYYY() fails appropriately", {
  expect_error(new_YYYY("PY2011")) # new_YYYY() doesn't accept prefixes (use YYYY() for that)
})
