context("gather_years")

test_data <-
  expand_grid(
    year = CY(2010:2012),
    cat_id = 288:289L) %>%
  mutate(
    ems_qty = as.double(1:n()))

expected_data <-
  tribble(
    ~ cat_id, ~ CY2010, ~ CY2011, ~ CY2012,
    288L, 1, 3, 5,
    289L, 2, 4, 6)

test_that("explicit `value_var` avoids use of `find_qty_var()`", {

  expected <-
    expected_data

  expect_silent(
    result <-
      test_data %>%
      spread_years(
        verbose = FALSE))

  expect_equivalent(
    result,
    expected)

})

test_that("if missing `value_var`, then default to result of `find_qty_var()`", {

  expect_error(
    result <-
      spread_years(
        mutate(test_data, tput_qty = 999),
        verbose = FALSE),
    "Which one")

  expect_silent(
    result <-
      spread_years(
        mutate(test_data, tput_qty = 999),
        value_var = ems_qty,
        verbose = FALSE))

  expect_equivalent_data(
    result,
    mutate(expected_data, tput_qty = 999))

})
