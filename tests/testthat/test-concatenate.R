test_that("c() works when timelines are identical", {
  x <- c(RY(2011), RY(2015:2018))
  expect_identical(x, RY(2011, 2015:2018))
})

test_that("c() fails when timelines are not identical", {
  expect_error(c(RY(2000), PY(2003:2004)))
})
