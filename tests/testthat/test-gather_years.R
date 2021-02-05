context("gather_years")

test_data <-
  tribble(
    ~ cat_id, ~ CY2011, ~ CY2012,
    101, 345.6, 432,
    102, 23.1, 98.0)

expected_data <-
  tibble(
    cat_id = c(101, 101, 102, 102),
    year = CY(2011, 2012, 2011, 2012),
    ems_qty = c(345.6, 432, 23.1, 98))

test_that("warn if missing `value_var` (then default to ems_qty)", {

  expected <-
    expected_data

  expect_warning(
    result <-
      test_data %>%
      gather_years(
        verbose = TRUE),
    "value_var")

  expect_equivalent_data(
    result,
    expected)

})

test_that("one symbol argument (NSE)", {

  expected <-
    expected_data %>%
    rename(
      foo = ems_qty)

  test_data %>%
    gather_years(
      foo,
      verbose = TRUE) %>%
    expect_equivalent_data(
      expected)

})

test_that("one unnamed character argument", {

  expected <-
    expected_data %>%
    rename(
      bar = ems_qty)

  test_data %>%
    gather_years(
      "bar",
      verbose = TRUE) %>%
    expect_equivalent_data(
      expected)

})

test_that("one variable argument (NSE)", {

  expected <-
    expected_data

  qty_var <- "ems_qty"

  test_data %>%
    gather_years(
      !!qty_var,
      verbose = TRUE) %>%
    expect_equivalent_data(
      expected)

})

