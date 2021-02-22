#' @note `as.character` just invokes `format`
#' @noRd
#' @export
as.character.YYYY <- function (x, ...) {
  #format(x, ...)
  attr(x, "timeline") <- NULL
  unclass(x)
}

#' @noRd
#' @export
as.integer.YYYY <- function (x, ...) {
  as.integer(str_remove(as.character(x), timeline(x)))
}

#' @param x `YYYY` object
#' @param tz character
#' @param ... ignored
#'
#' @noRd
#' @export
as.Date.YYYY <- function (x, ..., tz = "") {
  datestamp <- paste0(as.integer(x), "-01-01")
  as.Date(datestamp, tz = tz, ...)
}

#' @param x `YYYY` object
#' @param tz character
#' @param ... further arguments to [as.POSIXct()]
#'
#' @noRd
#' @export
as.POSIXct.YYYY <- function (x, tz = "", ...) {
  dttm <- ISOdatetime(as.integer(x), 01, 01, 00, 00, 00, tz = tz)
  as.POSIXct(dttm, tz = tz, ...)
}

#' @note supports regression and prediction use-cases
#' @noRd
#' @export
as.double.YYYY <- function (x, ...) {
  as.double(as.integer(x, ...))
}
