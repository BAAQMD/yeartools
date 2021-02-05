library(dplyr)
library(vctrs)

rm(list = ls())

#' @export
new_YYYY <- function(..., timeline = "CY") {
  year <- c(...)
  stopifnot(is.numeric(year))
  if (any(round(year) != year)) {
    stop("Only integer years are supported.")
  }
  if (any(year < 0)) {
    stop("Only positive integers are supported.")
  }
  new_rcrd(list(year = as.integer(year)), timeline = timeline, class = "YYYY")
}

#' @export
format.YYYY <- function (x, ...) {
  year <- field(x, "year")
  format(year, ...)
}

#' @export
print.YYYY <- function (x, ...) {
  x <- unclass(x)
  NextMethod()
}

#' @export
vec_ptype2.YYYY.YYYY <- function(x, y, ...) {
  x
}

#' @export
vec_cast.YYYY.YYYY <- function(x, to, ...) {
  x
}

df <- tibble(
  year = new_YYYY(2000:2004),
  fruit = c("Apple", "Apple", "Banana", "Cherry", "Cherry"),
  price = c(1, 1, 2, 3, 4))

group_by(df, year)
group_by(df, year, fruit)
