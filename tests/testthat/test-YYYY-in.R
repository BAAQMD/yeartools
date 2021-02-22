test_that("YYYY can be used to index a vector", {

  CONCORDANCE <- c(
    "BY2008" = "foo",
    "BY2011" = "bar",
    "BY2015" = "baz")

  expect_identical(
    unname(CONCORDANCE[[BY(2008)]]),
    "foo")

  expect_identical(
    unname(CONCORDANCE[BY(2011, 2015)]),
    c("bar", "baz"))

})
