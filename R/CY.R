#' @include YYYY.R

#' @family YYYY
#' @describeIn YYYY Calendar year(s)
#' @inheritParams YYYY
#'
#' @export
CY <- function (...) {
  yrs <- YYYY(..., prefix = "CY")
  structure(yrs, class = c("CY", "YYYY"))
}

#' @param x `CY` object
#'
#' @noRd
#' @export
as_CY <- function (x) {
  x <- structure(x, class = "CY")
  return(x)
}

#'----------------------------------------------------------------------

#' @param x `CY` object
#' @param i integer
#'
#' @export
`[.CY` <- function(x, i) {
  as_CY(NextMethod())
}

#'----------------------------------------------------------------------

#' @param x `CY` vector
#' @param ...
#'
#' @noRd
vec_ptype_abbr.CY <- function (x, ...) {
  "CY"
}
