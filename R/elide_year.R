#' elide_year
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#'
#' @param x `RY`, `PY`, `CY`, or `BY` object
#' @param verbose display messages
#'
#' @importFrom stringr str_match
#'
#' @export
elide_year <- function (
  x,
  pattern = "^([CRPB]Y)?([0-9]{4})$",
  verbose = getOption("verbose")
) {
  matches <- stringr::str_match(as.character(x), pattern = pattern)
  year <- as.integer(matches[, ncol(matches)])
  return(year)
}
