context("YYYY")

test_that("RY alone", {

  yrs <-
    RY(2011:2014)

  expect_true(
    length(yrs) == 4)

  expect_s3_class(
    yrs,
    c("RY", "YYYY"))

  expect_equivalent(
    as.character(yrs),
    paste0("RY", 2011:2014))

})

test_that("RY in tibble", {

  test_data <-
    tibble::tibble(
      year = RY(c(2011, 2011, 2012, 2012)),
      foo = 1:4,
      bar = c("A", "A", "B", "B"))

  grouped <-
    dplyr::group_by(test_data, year, bar)

  expect_s3_class(
    grouped$year,
    c("RY", "YYYY"))

  summarised <-
    dplyr::summarise(
      grouped, foo = sum(foo))

  expect_s3_class(
    summarised$year,
    c("RY", "YYYY"))

})
