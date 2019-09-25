context("unpack_years")

input_data <-
  tibble(
    years = c("RY(2007:2009)", "RY2001", "PY(2003)"),
    ems_qty = c(111, 22, 45))

# Note: we expect `years` (plural) to become `year` (singular)
expected <-
  tibble(
    year = c("RY2007", "RY2008", "RY2009", "RY2001", "PY2003"),
    ems_qty = c(111, 111, 111, 22, 45))

test_that("explicit year_var", {

  expect_equal(
    unpack_years(input_data, year_var = "years"),
    expected)

})

test_that("implicit year_var", {

  expect_equal(
    unpack_years(input_data),
    expected)

})

test_that("verbose", {

  expect_message(
    unpacked <- unpack_years(input_data, verbose = TRUE),
    "[unpack_years]")

})

