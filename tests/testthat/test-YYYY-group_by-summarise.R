test_that("group_by() works", {

  require(dplyr)
  require(tibble)

  test_data <-
    tibble(
      year = RY(2011, 2011, 2012, 2012),
      foo = 1:4,
      bar = c("A", "A", "B", "B"))

  grouped_data <- dplyr::group_by(test_data, year, bar)
  expect_is(grouped_data$year, "YYYY")

  summarised_data <- summarise(grouped_data, foo = sum(foo))
  expect_is(summarised_data$year, "YYYY")
  expect_setequal(summarised_data$year, RY(2011, 2012))

})
