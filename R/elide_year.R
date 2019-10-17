#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#'
#' @export
elide_year <- function (
  x,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[elide_year] ", ...)

  x <- as.character(x)

  prefixes <-
    stringr::str_extract(x, "^[A-Z]Y") %>%
    unique()

  if (!all_true(is.na(prefixes))) {
    msg("dropping ", strtools::str_csv(prefixes), " from year")
  }

  parsed <-
    parse_year(x)

  return(parsed)
}
