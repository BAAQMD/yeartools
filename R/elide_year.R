#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param verbose display messages
#'
#' @export
elide_year <- function (
  x,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[elide_year] ", ...)

  x <- as.character(x)

  prefixes <-
    unique(
      stringr::str_extract(x, "^[A-Z]Y"))

  if (isFALSE(funtools::all_true(is.na(prefixes)))) {
    msg("dropping ", strtools::str_csv(prefixes), " from year")
  }

  parsed <-
    parse_year(x)

  return(parsed)
}
