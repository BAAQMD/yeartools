test_that("c() works when timelines are identical", {

  x <- c(RY(2011), RY(2015:2018))
  expect_equal(x, RY(2011, 2015:2018))

})

test_that("c() coerces to character when timelines are not identical", {

  expect_warning(
    x <- c(RY(2000), PY(2003:2004)),
    "character")

  expect_equal(x, c("RY2000", "PY2003", "PY2004"))

})
