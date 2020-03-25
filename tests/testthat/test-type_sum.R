context("type_sum")

test_that("type_sum works", {

  expect_equal(
    type_sum(CY(2015:2018)),
    "CY")

  expect_equal(
    type_sum(RY(2015:2018)),
    "RY")

})

test_that("tibble display works", {

  test_data <-
    tibble::tibble(
      year = CY(2015:2018))

})
