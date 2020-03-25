#' @name years
#' @rdname years
#'
#' Years (the DataBank way)
#'
#' This family of functions helps to index requests for DataBank "files".
#'
#' @note **FIXME: more and better description.** Why do we need these? What are they for?
#'
#' @param yyyy four-digit integer(s)
#'
#' @seealso [DB_XXXX_CONCORDANCE]
#'
#' @examples
#' # The constructors `RY()`, `PY()`, etc. all yield instances of a very simple "S3" class (`YYYY`).
#' CY(2011:2014) %>% inherits("YYYY")
#' PY(2011:2014) %>% as.character()
#' BY(2011:2014) %>% as.integer()
#' PY(2011:2014) - 1 # can add and subtract
#' CY(2011:2014) %>% max()
#'
NULL

#' @noRd
#' @export
YYYY <- function (..., prefix) {
  yyyy <- c(...)
  stopifnot(is.numeric(yyyy))
  x <- paste0(prefix, yyyy)
  x <- as_YYYY(x)
  return(x)
}

#' @noRd
#' @export
as_YYYY <- function (x) {
  x <- structure(x, class = "YYYY")
  return(x)
}

#'----------------------------------------------------------------------

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

#'----------------------------------------------------------------------

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

#'----------------------------------------------------------------------

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

#'----------------------------------------------------------------------

#' @noRd
print.YYYY <- function (yyyy, ...) {
  cat(as.character(yyyy))
}

#'----------------------------------------------------------------------

#' @export
`[.YYYY` <- function(x, i) {
  as_YYYY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
#' @export
type_sum.YYYY <- function (yyyy, ...) {
  "YYYY"
}
