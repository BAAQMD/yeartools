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

test_that("as.numeric works", {
  expect_equal(
    as.numeric(CY(2015:2018)),
    as.numeric(2015:2018))
})
