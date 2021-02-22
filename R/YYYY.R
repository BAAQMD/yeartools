library(vctrs)
library(tidyverse)

new_YYYY <- function (
  x,
  ...,
  timeline = c("BY", "CY", "RY", "PY"),
  pattern = "^([CRPB]Y)?([0-9]{4})$"
) {

  x <- c(x, ...)
  if (is.numeric(x)) {
    stopifnot(all(x == round(x)))
    year <- as.integer(x)
    timeline <- match.arg(timeline)
  } else if (is.character(x)) {
    matches <- stringr::str_match(x, pattern)
    year <- as.integer(matches[, 3])
    timeline <- unique(matches[, 2])
    stopifnot(length(timeline) == 1)
  }

  rcrd <- new_rcrd(list(year = year), class = "YYYY")
  attr(rcrd, "timeline") <- timeline
  return(rcrd)

}

RY <- function (x, ...) {
  new_YYYY(x, ..., timeline = "RY")
}

PY <- function (x, ...) {
  new_YYYY(x, ..., timeline = "PY")
}

BY <- function (x, ...) {
  new_YYYY(x, ..., timeline = "BY")
}

CY <- function (x, ...) {
  new_YYYY(x, ..., timeline = "CY")
}
