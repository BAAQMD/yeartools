require(tibble)

test_that("arrange() works on one column", {
  df <- tibble(year = CY(2002, 2000), fruit = c("apple", "pear"))
  arranged <- arrange(df, year)
  expected <- tibble(year = CY(2000, 2002), fruit = c("pear", "apple"))
  expect_identical(arranged, expected)
})

test_that("arrange() works on two columns", {
  df <- tibble(year = CY(2002, 2000, 2000), fruit = c("apple", "pear", "banana"))
  arranged <- arrange(df, year, fruit)
  expected <- tibble(year = CY(2000, 2000, 2002), fruit = c("banana", "pear", "apple"))
  expect_identical(arranged, expected)
})

test_that("arrange() works on two columns when YYYY is cast to character", {
  df <- tibble(year = CY(2002, 2000, 2000), fruit = c("apple", "pear", "banana"))
  arranged <- arrange(df, as.character(year), fruit)
  expected <- tibble(year = CY(2000, 2000, 2002), fruit = c("banana", "pear", "apple"))
  expect_identical(arranged, expected)
})
