library(vctrs)
library(pillar)


RY <- function (x) {
  new_YYYY(x, timeline = "RY")
}

PY <- function (x) {
  new_YYYY(x, timeline = "PY")
}

BY <- function (x) {
  new_YYYY(x, timeline = "BY")
}

#' @export
format.YYYY <- function(x, ...) {
  x_valid <- which(!is.na(x))
  year <- field(x, "year")[x_valid]
  ret <- rep(NA_character_, vec_size(x))
  ret[x_valid] <- format(year)
  return(ret)
}

vec_ptype_abbr.YYYY <- function (x, ...) {
  return(attr(x, "timeline"))
}

#' @export
print.YYYY <- function (x, ...) {
  cat("<", attr(x, "timeline"), ">", sep = "")
  x <- field(x, "year")
  NextMethod()
}

(foo <- RY(2000:2002))
(bar <- tibble::tibble(year = foo))
