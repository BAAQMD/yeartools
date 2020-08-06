#' YYYY
#'
#' This family of functions helps to index requests for DataBank "files".
#'
#' @param ... integer(s)
#' @param prefix character
#'
#' @note **FIXME: more and better description.** Why do we need these? What are they for?
#'
#' @seealso [DB_XXXX_CONCORDANCE]
#'
#' @examples
#' # The constructors `RY()`, `PY()`, etc. all yield instances of a very simple "S3" class (`YYYY`).
#' CY(2011:2014) %>% inherits("YYYY")
#' PY(2011:2014) %>% as.character()
#' BY(2011:2014) %>% as.integer()
#' CY(2011:2014) %>% max()
#'
#' @export
YYYY <- function (..., prefix) {
  x <- c(...)
  stopifnot(is.numeric(x))
  x <- paste0(prefix, x)
  x <- as_YYYY(x)
  return(x)
}

#' @param x `YYYY` object
#'
#' @noRd
#' @export
as_YYYY <- function (x) {
  x <- structure(x, class = "YYYY")
  return(x)
}

#'----------------------------------------------------------------------

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
#' @export
as.character.YYYY <- function (x, ...) {
  unclass(x)
}

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
#' @export
as.integer.YYYY <- function (x, ...) {
  readr::parse_number(x, ...)
}

#' @param x `YYYY` object
#' @param tz character
#' @param ... ignored
#'
#' @noRd
#' @export
as.Date.YYYY <- function (x, ..., tz = "") {
  datestamp <- stringr::str_c(as.integer(x), "-01-01")
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

#' @noRd
#' @export
as.double.YYYY <- function (x, ...) {
  #dttm <- as.POSIXct(x, ...)
  #as.double(dttm)
  as.double(as.integer(x, ...))
}

#'----------------------------------------------------------------------

#' @param e1 `YYYY` object
#' @param e2 `YYYY` object
#'
#' @noRd
`+.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  cls <- dplyr::first(class(e1))
  do.call(cls, list(x = as.integer(e1) + e2))
}

#' @param e1 `YYYY` object
#' @param e2 `YYYY` object
#'
#' @noRd
`-.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  cls <- dplyr::first(class(e1))
  do.call(cls, list(x = as.integer(e1) - e2))
}

#'----------------------------------------------------------------------

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
min.YYYY <- function (x, ...) {
  values <- as.integer(x)
  i <- which.min(values)
  cls <- dplyr::first(class(x))
  do.call(cls, list(x = values[i]))
}

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
max.YYYY <- function (x, ...) {
  values <- as.integer(x)
  i <- which.max(values)
  cls <- dplyr::first(class(x))
  do.call(cls, list(x = values[i]))
}

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
range.YYYY <- function (x, ...) {
  values <- as.integer(x)
  i <- c(which.min(values), which.max(values))
  cls <- dplyr::first(class(x))
  do.call(cls, list(x = values[i]))
}

#'----------------------------------------------------------------------

#' @param x `YYYY` object
#' @param ...
#'
#' @noRd
print.YYYY <- function (x, ...) {
  cat(as.character(x))
}

#'----------------------------------------------------------------------

#' @param x `YYYY` object
#' @param i integer
#'
#' @export
`[.YYYY` <- function(x, i) {
  as_YYYY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
vec_ptype_abbr.YYYY <- function (x, ...) {
  "YYYY"
}
