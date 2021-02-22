require(tibble)

test_that("bind_rows() works when both are CY", {
  df1 <- tibble(year = CY(2000:2009))
  df2 <- tibble(year = CY(2010:2020))
  expect_equal(timeline(bind_rows(df1, df2)$year), "CY")
})

test_that("bind_rows() works when both are RY", {
  df1 <- tibble(year = RY(2000:2009))
  df2 <- tibble(year = RY(2010:2020))
  expect_equal(timeline(bind_rows(df1, df2)$year), "RY")
})

test_that("bind_rows() works when both are PY", {
  df1 <- tibble(year = PY(2000:2009))
  df2 <- tibble(year = PY(2010:2020))
  expect_equal(timeline(bind_rows(df1, df2)$year), "PY")
})

test_that("bind_rows() fails to combine RY and PY", {

  df1 <- tibble(year = RY(2000:2009))
  df2 <- tibble(year = PY(2010:2020))
  expect_error(
    bind_rows(df1, df2),
    class = "vctrs_error_incompatible_type")

})

test_that("bind_rows() fails to combine RY and CY", {

  df1 <- tibble(year = RY(2000:2009))
  df2 <- tibble(year = CY(2010:2020))
  expect_error(
    bind_rows(df1, df2),
    class = "vctrs_error_incompatible_type")

})
