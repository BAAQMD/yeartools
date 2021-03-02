library(vctrs)
library(tidyverse)

new_YYYY <- function (
  x = character(),
  timeline = NULL
) {
  result <- vctrs::new_vctr(x, timeline = timeline, class = c("YYYY", "character"))
  return(result)
}

#' Typically we should call `YYYY()` rather than `new_YYYY()`.
#'
#'
#'
#' @export
#' @noRd
YYYY <- function(
  x = character(),
  ...,
  timeline = NULL,
  pattern = "^([CRPB]Y)?([0-9]{4})$",
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[YYYY] ", ...)

  x <- c(x, ...)

  # For a zero-length `x`, just short-circuit and invoke `new_YYYY()`;
  # don't bother trying to get fancy by parsing `x`.
  if (length(x) == 0) {
    msg("x is empty; short-circuiting")
    result <- new_YYYY(x, timeline = timeline, class = c("YYYY", "character"))
    return(result)
  }

  # If we got a numeric `x`, just cast it to character, and then keep going.
  if (is.numeric(x)) {
    msg("x is numeric; converting to character")
    stopifnot(all(x == round(x)))
    x <- as.character(as.integer(x))
  }

  # If we got a `YYYY` argument, then we just need to check that the timelines
  # are compatible. As of today (2021-03-02), there's no support for automatically
  # aligning different timelines, so we throw an error if they're different.
  # When the timelines are identical, we just return `x` unchanged.
  if (inherits(x, "YYYY")) {
    msg("x is a YYYY object")
    if (timeline == attr(x, "timeline")) {
      return(x) # nothing to do
    } else {
      err_msg <- paste0("can't automatically align ", attr(x, "timeline"), " with ", timeline)
      stop(err_msg)
    }
  }

  # If we got here, then `x` should be a character vector.
  # Now, we split `x` into two parts: `prefix` and `years`.
  # `prefix` might be nonexistent, which is OK so long as `timeline` was supplied.
  stopifnot(is.character(x))
  matches <- stringr::str_match(x, pattern)
  prefix <- unique(na.omit(matches[, 2]))
  years <- matches[, 3]

  # If an explicit `timeline` was supplied, then we'll use that.
  if (isFALSE(is.null(timeline))) {
    msg("timeline explicitly supplied: ", timeline)
    # The `timeline` argument takes precedence over any prefix(es) embedded in `x`,
    # so let's warn the user, as a courtesy, if the
    if (length(prefix) > 0) {
      if (all(prefix == timeline)) {
        # pass; this is OK
      } else {
        warning("both a timeline and some (non-matching) prefix(es) were supplied; ignoring prefix(es)")
      }
    }
    result <- new_YYYY(years, timeline = timeline)
    return(result)
  }

  if (isTRUE(is.null(timeline))) {
    msg("timeline not supplied; prefix is: ", prefix)
    if (length(prefix) > 0) {
      stop("more than one unique prefix is present in x")
    }
    result <- new_YYYY(years, timeline = prefix)
    return(result)
  }

  stop("shouldn't get here")

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
