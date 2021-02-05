#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#' Drops the timeline (that is, prefix) and returns a simple integer.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param verbose logical
#'
#' @importFrom stringr str_extract
#' @import
#'
#' @export
elide_year <- function (
  x,
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

  return(as.integer(x))

}
