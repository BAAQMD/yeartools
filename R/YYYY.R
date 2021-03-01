library(vctrs)
library(tidyverse)

new_YYYY <- function (
  x = character(),
  timeline = c("BY", "CY", "RY", "PY"),
  pattern = "^([CRPB]Y)?([0-9]{4})$"
) {

  stopifnot(all(stringr::str_detect(x, pattern)))

  if (is.numeric(x)) {

    stopifnot(all(x == round(x)))
    x <- as.character(as.integer(x))
    timeline <- match.arg(timeline)

  } else if (is.character(x)) {

    matches <- stringr::str_match(x, pattern)

    if (is.null(timeline)) {
      if (isTRUE(all(is.na(matches[, 2])))) {
        stop("must supply timeline or prefixed years")
      } else {
        timeline <- unique(na.omit(matches[, 2]))
        if (length(timeline) != 1) {
          stop("more than one unique prefix detected")
        }
      }
    } else {
      if (inherits(x, "YYYY")) {
        if (attr(x, "timeline") != timeline) {
          err_msg <- paste0("can't automatically align ", attr(x, "timeline"), " with ", timeline)
          stop(err_msg)
        }
      }
      if (isFALSE(all(is.na(matches[, 2])))) {
        if (isFALSE(identical(timeline, unique(matches[, 2])))) {
          stop("both a timeline and some prefix(es) were supplied")
        }
      }
      timeline <- match.arg(timeline)
    }

    x <- matches[, 3]

  }

  result <- vctrs::new_vctr(x, timeline = timeline, class = c("YYYY", "character"))

  return(result)

}

#' @export
#' @noRd
YYYY <- function(x = character(), ..., timeline = NULL) {
  x <- c(x, ...)
  new_YYYY(x, timeline = timeline)
}

#' @export
#' @noRd
RY <- function (x = character(), ...) {
  YYYY(x, ..., timeline = "RY")
}

#' @export
#' @noRd
PY <- function (x = character(), ...) {
  YYYY(x, ..., timeline = "PY")
}

#' @export
#' @noRd
BY <- function (x = character(), ...) {
  YYYY(x, ..., timeline = "BY")
}

#' @export
#' @noRd
CY <- function (x = character(), ...) {
  YYYY(x, ..., timeline = "CY")
}
