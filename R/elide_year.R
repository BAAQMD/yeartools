#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#' Drops the timeline (that is, prefix) and returns a simple integer.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param ... reserved for future use
#'
#' @note `elide_years()` is a backwards-compatible alias for `elide_year()`.
#' @aliases elide_years
#'
#' @importFrom stringr str_match
#'
#' @export
elide_year <- function (x, ...) {
  UseMethod("elide_year")
}

#' @rdname elide_year
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#'
#' @export
elide_year.default <- function (x, ...) {
  pattern <- "^([CRPB]Y)?([0-9]{4})$"
  matches <- stringr::str_match(as.character(x), pattern = pattern)
  elided <- matches[, ncol(matches)]
  return(elided)
}

#' @rdname elide_year
#'
#' @param x data.frame
#' @param ... reserved for future use
#'
#' @export
elide_year.data.frame <- function (x, ...) {
  if ("year" %in% names(x)) {
    result <- dplyr::mutate(x, year = elide_year(as.character(year)))
  } else {
    warning("[elide_year] column `year` is not in your data")
    result <- x
  }
  return(result)
}

#' @describeIn elide_year alias for `elide_year()`
#' @export
#' @noRd
elide_years <- elide_year
