#' @include YYYY.R

#' @family YYYY
#' @describeIn years Permit year(s)
#' @inheritParams YYYY
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
vec_ptype_abbr.PY <- function (PY, ...) {
  "PY"
}
