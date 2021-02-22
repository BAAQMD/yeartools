#' @name YYYY
#'
#' @title Years with explicit timelines
#'
#' @description
#' This family of functions helps to index requests for DataBank "files",
#'     and helps with the construction of "base year" emission inventories.
#'
#' The constructors `RY()`, `PY()`, etc. all yield instances of a very simple "S3" class (`YYYY`).
#'
#' @references
#' - [What's In a Year?](https://paper.dropbox.com/doc/Key-Concept-Whats-in-a-Year--BEm7K0si5VGV_nTugk5BHqo0Ag-p6bICw8OZNDcUazyCFULw)
#'
NULL

#' new_YYYY
#'
#' @rdname YYYY
#'
#' @param ... positive whole numbers
#' @param timeline character
#'
#' @export
new_YYYY <- function (
  ...,
  timeline = c("CY", "RY", "PY", "BY"),
  pattern = "^([CRPB]Y)([0-9]{4})$"
) {

  if (missing(timeline)) {
    x <- c(...)
    matches <- stringr::str_match(as.character(x), pattern)
    timeline <- unique(matches[, 2])
    stopifnot(length(timeline) == 1)
    year <- matches[, 3]
  } else {
    timeline <- match.arg(timeline)
    year <- c(...)
  }

  year <- as.integer(year)

  if (isFALSE(is.numeric(year)) || isTRUE(any(year != round(year)))) {
    err_msg <- "Only positive whole-number years are supported."
    stop(err_msg)
  }

  x <- paste0(timeline, year)
  class(x) <- c("YYYY", "character")

  attr(x, "timeline") <- timeline
  return(x)

}

#' @describeIn YYYY Calendar year(s)
#' @export
CY <- function (...) {
  new_YYYY(..., timeline = "CY")
}

#' @describeIn YYYY Reporting year(s)
#' @export
RY <- function (...) {
  new_YYYY(..., timeline = "RY")
}

#' @describeIn YYYY Permit year(s)
#' @export
PY <- function (...) {
  new_YYYY(..., timeline = "PY")
}

#' @describeIn YYYY Base year(s)
#' @export
BY <- function (...) {
  new_YYYY(..., timeline = "BY")
}
