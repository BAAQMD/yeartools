test_that("left_join() works", {
  foo_data <- tibble(year = RY(2011), foo = "foo")
  bar_data <- tibble(year = "RY2011", bar = "bar")
  joined_data <- left_join(foo_data, bar_data, by = "year")
})
