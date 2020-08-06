expect_equivalent_data <- function (a, b) {
  expect_true(dplyr::all_equal(a, b, ignore_row_order = TRUE))
}
