#' @noRd
#' @export
as.character.YYYY <- function (from) {
  unclass(from)
}

#' @noRd
#' @export
as.integer.YYYY <- function (from) {
  readr::parse_number(from)
}

#' @noRd
#' @export
as.Date.YYYY <- function (from, ..., tz = "") {
  datestamp <- str_c(as.integer(from), "-01-01")
  as.Date(datestamp, tz = tz, ...)
}

#' @noRd
#' @export
as.POSIXct.YYYY <- function (from, tz = "") {
  dttm <- ISOdatetime(as.integer(from), 01, 01, 00, 00, 00, tz = tz)
  as.POSIXct(dttm, tz = tz)
}

#' @noRd
#' @export
as.double.YYYY <- function (from, ...) {
  dttm <- as.POSIXct(from, ...)
  as.double(dttm)
}

#' @noRd
print.YYYY <- function (yyyy, ...) {
  cat(as.character(yyyy))
}

#' @noRd
`+.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  cls <- dplyr::first(class(e1))
  do.call(cls, list(yyyy = as.integer(e1) + e2))
}

#' @noRd
`-.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  cls <- dplyr::first(class(e1))
  do.call(cls, list(yyyy = as.integer(e1) - e2))
}

#' @noRd
type_sum.YYYY <- function (yyyy, ...) {
  dplyr::first(class(yyyy))
}

#' @noRd
min.YYYY <- function (yyyy, ...) {
  values <- as.integer(yyyy)
  i <- which.min(values)
  cls <- dplyr::first(class(yyyy))
  do.call(cls, list(yyyy = values[i]))
}

#' @noRd
max.YYYY <- function (yyyy, ...) {
  values <- as.integer(yyyy)
  i <- which.max(values)
  cls <- dplyr::first(class(yyyy))
  do.call(cls, list(yyyy = values[i]))
}

#' @noRd
range.YYYY <- function (yyyy, ...) {
  values <- as.integer(yyyy)
  i <- c(which.min(values), which.max(values))
  cls <- dplyr::first(class(yyyy))
  do.call(cls, list(yyyy = values[i]))
}
