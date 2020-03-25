#' @include YYYY.R

#' @family years
#' @describeIn years Reporting year(s)
#' @inheritParams years
#'
#' @export
RY <- function (...) {
  yrs <- YYYY(..., prefix = "RY")
  structure(yrs, class = c("RY", "YYYY"))
}

#' @noRd
#' @export
as_RY <- function (x) {
  x <- structure(x, class = "RY")
  return(x)
}

#'----------------------------------------------------------------------

#' @export
`[.RY` <- function(x, i) {
  as_RY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
#' @export
type_sum.RY <- function (RY, ...) {
  "RY"
}
