x <- CY(2000, 2010:2011)

test_that("as.character() works", {

  expect_identical(
    as.character(x),
    c("CY2000", "CY2010", "CY2011"))

})

test_that("as.integer() gives warning", {

  expect_warning(as.integer(x))

})
