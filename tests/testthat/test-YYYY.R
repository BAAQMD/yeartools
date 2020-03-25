test_that("as.character works", {
  expect_equal(
    as.character(CY(2015:2018)),
    str_c("CY", 2015:2018))
})

test_that("as.integer works", {
  expect_equal(
    as.integer(CY(2015:2018)),
    as.integer(2015:2018))
})

test_that("as.Date works", {

  expect_equal(
    as.Date(CY(2015:2018)),
    as.Date(str_c(2015:2018, "-01-01")))

  expect_equal(
    as.Date(CY(2015:2018), tz = Sys.timezone()),
    as.Date(str_c(2015:2018, "-01-01"), tz = Sys.timezone()))

})

test_that("as.POSIXct works", {

  expect_equal(
    as.POSIXct(CY(2015:2018)),
    as.POSIXct(str_c(2015:2018, "-01-01")))

  expect_equal(
    as.POSIXct(CY(2015:2018), tz = Sys.timezone()),
    as.POSIXct(str_c(2015:2018, "-01-01"), tz = Sys.timezone()))

})

test_that("as.numeric works", {

  expect_equal(
    as.numeric(CY(2015:2018)),
    as.numeric(as.POSIXct(str_c(2015:2018, "-01-01"))))

})
