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
as.numeric.YYYY <- function (from) {
  as.integer.YYYY(from)
}

# library(tibble)

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
