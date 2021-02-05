library(magrittr)

test_that("S3 method: default", {

  RY(2007:2009) %>%
    elide_years() %>%
    expect_identical(
      as.integer(c(2007:2009)))

})

test_that("S3 method: data.frame", {

  input_data <-
    tibble(
      year = c(RY(2007:2009), RY(2001), PY(2003)),
      ems_qty = c(rep(111, 3), 22, 45))

  expected <-
    tibble(
      year = as.integer(c(2007:2009, 2001, 2003)),
      ems_qty = c(rep(111, 3), 22, 45))

  input_data %>%
    elide_years() %>%
    expect_equal(
      expected)

})

