test_that("format() works", {

  expect_equal(
    format(CY(2000:2002)),
    paste0("CY", 2000:2002))

  expect_equal(
    format(RY(2005:2009)),
    paste0("RY", 2005:2009))

  expect_equal(
    format(PY(2005:2009)),
    paste0("PY", 2005:2009))

  expect_equal(
    format(BY(2011)),
    paste0("BY", 2011))

})
