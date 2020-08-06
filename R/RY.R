#' @include YYYY.R

#' @family YYYY
#' @describeIn YYYY Reporting year(s)
#' @inheritParams YYYY
#'
#' @export
RY <- function (...) {
  yrs <- YYYY(..., prefix = "RY")
  structure(yrs, class = c("RY", "YYYY"))
}

#' @noRd
#' @export
as_RY <- function (x) {
  x <- structure(x, class = c("RY", "YYYY"))
  return(x)
}

#'----------------------------------------------------------------------

#' @export
`[.RY` <- function(x, i) {
  as_RY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
vec_ptype_abbr.RY <- function (RY, ...) {
  "RY"
}
