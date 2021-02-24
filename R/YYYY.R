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

  result <- paste0(timeline, year)
  class(result) <- c("YYYY", "character")
  return(result)

}

#' @export
#' @noRd
RY <- function (x, ...) {
  x <- c(x, ...)
  stopifnot(all(stringr::str_detect(x, "^[0-9]{4}$")))
  new_YYYY(x, timeline = "RY")
}

#' @export
#' @noRd
PY <- function (x, ...) {
  x <- c(x, ...)
  stopifnot(all(stringr::str_detect(x, "^[0-9]{4}$")))
  new_YYYY(x, timeline = "PY")
}

#' @export
#' @noRd
BY <- function (x, ...) {
  x <- c(x, ...)
  stopifnot(all(stringr::str_detect(x, "^[0-9]{4}$")))
  new_YYYY(x, timeline = "BY")
}

#' @export
#' @noRd
CY <- function (x, ...) {
  x <- c(x, ...)
  stopifnot(all(stringr::str_detect(x, "^[0-9]{4}$")))
  new_YYYY(x, timeline = "CY")
}
