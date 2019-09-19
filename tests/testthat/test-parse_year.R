context("parse_year")

test_that("parse_year.character", {

  expect_equal(
    parse_year("CY2009"),
    2009L)

  expect_equal(
    parse_year("2011"),
    2011L)

  expect_error(
    parse_year("123"),
    "four digits")

})

test_that("parse_year.integer", {

  expect_equal(
    parse_year(1999L),
    1999L)

})

test_that("parse_year.YYYY", {

  expect_equal(
    parse_year(CY(2030)),
    2030L)

  expect_equal(
    parse_year(RY(2015)),
    2015L)

  expect_equal(
    parse_year(PY(2019)),
    2019L)

  expect_equal(
    parse_year(BY(2011)),
    2011L)

})
