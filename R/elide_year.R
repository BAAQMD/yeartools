#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#' Drops the timeline (that is, prefix) and returns a simple integer.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param verbose logical
#'
#' @importFrom stringr str_extract
#'
#' @export
elide_year <- function (
  x,
  pattern = "([CRPB]Y)?([0-9]{4})",
  verbose = getOption("verbose")
) {

  # msg <- function (...) if(isTRUE(verbose)) message("[elide_year] ", ...)
  #
  # x <- as.character(x)
  #
  # timelines <-
  #   unique(
  #     stringr::str_extract(
  #       as.character(x),
  #       "^[A-Z]Y"))
  #
  # if (isFALSE(all(is.na(timelines)))) {
  #   msg("dropping timelines (that is, prefixes) from year")
  # }

  if (inherits(x, "YYYY")) {

    return(as.integer(x))

  } else if (is.numeric(x)) {

    return(as.integer(round(x, digits = 0)))

  } else {

    matches <- stringr::str_match(as.character(x), pattern)
    return(as.integer(matches[, 3]))

  }

}
