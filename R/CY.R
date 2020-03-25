#' @include YYYY.R

#' @family years
#' @describeIn years Calendar year(s)
#' @inheritParams years
#'
#' @export
CY <- function (...) {
  yrs <- YYYY(..., prefix = "CY")
  structure(yrs, class = c("CY", "YYYY"))
}

#' @noRd
#' @export
as_CY <- function (x) {
  x <- structure(x, class = "CY")
  return(x)
}

#'----------------------------------------------------------------------

#' @export
`[.CY` <- function(x, i) {
  as_CY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
#' @export
type_sum.CY <- function (CY, ...) {
  "CY"
}
