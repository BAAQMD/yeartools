context("expand_years")

test_that("handles a single varying ancillary attribute", {

  input_data <-
    data.frame(
      year = c(2012, 2015),
      foo = c("bar", "baz"))

  expanded <-
    expand_years(
      input_data,
      2011:2017)

  expect_equal(
    expanded,
    data.frame(
      year = rep(2011:2017L, 2),
      foo = c(rep("bar", 7), rep("baz", 7))))

})

test_that("handles multiple constant ancillary attributes", {

  input_data <-
    data.frame(
      year = c(2012, 2015),
      foo = "bar",
      baz = "bap")

  expect_silent(
    expanded <-
      expand_years(
        input_data,
        2012:2015))

  expect_equal(
    nrow(expanded),
    length(2012:2015))

  expect_equal(
    expanded,
    data.frame(
      year = 2012:2015L,
      foo = "bar",
      baz = "bap"))

})


# test_that("real example (category #749)", {
#
#   input_data <-
#     tibble(
#       year = c("CY1975", "CY1988", "CY1989", "CY1990", "CY1991", "CY1992", "CY1993", "CY1994", "CY1995", "CY1996", "CY1997", "CY1998", "CY1999", "CY2000", "CY2001", "CY2002", "CY2003", "CY2004", "CY2005", "CY2006", "CY2007", "CY2008", "CY2009", "CY2010"),
#       cat_id = 749L,
#       pol_id = 1990L)
#
#   MIN_YEAR <- 1990L
#   MAX_YEAR <- 2030L
#
#   expand_years(
#     input_data,
#     seq(MIN_YEAR, MAX_YEAR))
#
# })
