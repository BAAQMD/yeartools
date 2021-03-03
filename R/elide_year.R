#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#' Drops the timeline (that is, prefix) and returns a simple integer.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param verbose logical
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

#' @export
elide_year.default <- function (
  x,
  pattern = "^([CRPB]Y)?([0-9]{4})$",
  verbose = getOption("verbose")
) {
  matches <- stringr::str_match(as.character(x), pattern = pattern)
  elided <- matches[, ncol(matches)]
  return(elided)
}

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

#' elide_years
#' @noRd
elide_years <- elide_year

#' elide_years
#' @noRd
elide_years <- elide_year
