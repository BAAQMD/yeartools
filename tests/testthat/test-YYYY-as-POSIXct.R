test_that("as.POSIXct works", {

  expect_equal(
    as.POSIXct(CY(2015:2018)),
    as.POSIXct(paste0(2015:2018, "-01-01")))

  expect_equal(
    as.POSIXct(CY(2015:2018), tz = Sys.timezone()),
    as.POSIXct(paste0(2015:2018, "-01-01"), tz = Sys.timezone()))

})
