#' Number of days in a year, including leap days
#'
#' @param year four-digit year (character, numeric, or integer)
#' @return integer
#' @importFrom lubridate leap_year
#' @export
ndays_in <- function (year) {
  tryCatch(365 + lubridate::leap_year(as.integer(year)),
           error = function (e) NULL)
}
