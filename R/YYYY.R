library(vctrs)
library(tidyverse)

new_YYYY <- function(x) {
  new_rcrd(
    list(year = x),
    class = "YYYY")
}

test_data <-
  tibble::tibble(
    year = new_YYYY(c(2011, 2011, 2012, 2012)),
    foo = 1:4,
    bar = c("A", "A", "B", "B"))

# Grouping by `year` alone works
grouped_by_year <- group_by(test_data, year)

# Grouping by `foo` and `bar works (neither are vctrs)
grouped_by_foo_and_bar <- group_by(test_data, foo, bar)

# Grouping by a combination of `year` (vctrs) and `foo` (numeric) fails
grouped_by_year_and_foo <- group_by(test_data, year, foo)

rlang::last_error()
rlang::last_trace()
