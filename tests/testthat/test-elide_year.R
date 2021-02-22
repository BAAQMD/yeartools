test_that("elide_year() works", {

  expect_identical(
    elide_year(CY(1995:1997)),
    c(1995L, 1996L, 1997L))

  expect_identical(
    elide_year(paste0("CY", 1995:1997)),
    c(1995L, 1996L, 1997L))

  expect_identical(
    elide_year(1995:1997),
    c(1995L, 1996L, 1997L))

  expect_identical(
    elide_year(as.integer(1995:1997)),
    c(1995L, 1996L, 1997L))

})
