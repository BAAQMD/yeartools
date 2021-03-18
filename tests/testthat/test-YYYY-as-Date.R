test_that("as.Date works", {

  expect_equal(
    as.Date(CY(2015:2018)),
    as.Date(paste0(2015:2018, "-01-01")))

  expect_equal(
    as.Date(CY(2015:2018), tz = Sys.timezone()),
    as.Date(paste0(2015:2018, "-01-01"), tz = Sys.timezone()))

})
