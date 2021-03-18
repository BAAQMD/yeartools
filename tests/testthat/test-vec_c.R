test_that("vec_c works", {
  x <- vec_c(RY(2011), RY(2012))
  expect_is(x, c("YYYY", "character"))
  expect_equal(timeline(x), "RY")
  expect_equivalent(x, RY(2011, 2012))
})
