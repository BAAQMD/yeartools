test_that("elide_year() works on vectors", {

  expect_identical(
    elide_year(CY(1995:1997)),
    as.character(c(1995L, 1996L, 1997L)))

  expect_identical(
    elide_year(paste0("CY", 1995:1997)),
    as.character(c(1995L, 1996L, 1997L)))

  expect_identical(
    elide_year(1995:1997),
    as.character(c(1995L, 1996L, 1997L)))

  expect_identical(
    elide_year(as.integer(1995:1997)),
    as.character(c(1995L, 1996L, 1997L)))

})

test_that("elide_year() works on tibble", {

  input_data <-
    tibble(
      year = c(RY(2007:2009), RY(2001), RY(2003)),
      ems_qty = c(rep(111, 3), 22, 45))

  expected <-
    tibble(
      year = as.character(c(2007:2009, 2001, 2003)),
      ems_qty = c(rep(111, 3), 22, 45))

  input_data %>%
    elide_years() %>%
    expect_equal(
      expected)

})

