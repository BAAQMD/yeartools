context("expand_years")

test_that("doesn't throw cols() warning", {

  test_data <-
    data.frame(
      year = c(2012, 2015),
      foo = c("bar", "baz"))

  expect_silent(
    expanded <-
      expand_years(
        test_data,
        2011:2017))

  expect_equal(
    expanded,
    data.frame(
      year = rep(2011:2017L, 2),
      foo = c(rep("bar", 7), rep("baz", 7))))

})
