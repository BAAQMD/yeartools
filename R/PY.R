#' @include YYYY.R

#' @family years
#' @describeIn years Permit year(s)
#' @inheritParams years
#'
#' @export
PY <- function (...) {
  yrs <- YYYY(..., prefix = "PY")
  structure(yrs, class = c("PY", "YYYY"))
}

#' @noRd
#' @export
as_PY <- function (x) {
  x <- structure(x, class = "PY")
  return(x)
}

#'----------------------------------------------------------------------

#' @export
`[.PY` <- function(x, i) {
  as_PY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
#' @export
type_sum.PY <- function (PY, ...) {
  "PY"
}
