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
    str_extract(x, "^[A-Z]Y") %>%
    unique()

  if (!all_true(is.na(prefixes))) {
    msg("dropping ", str_csv(prefixes), " from year")
  }

  parsed <-
    parse_year(x)

  return(parsed)
}
