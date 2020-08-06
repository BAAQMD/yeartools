#' @include YYYY.R

#' @family YYYY
#' @describeIn YYYY Base year(s)
#' @inheritParams YYYY
#'
#' @export
BY <- function (...) {
  yrs <- YYYY(..., prefix = "BY")
  structure(yrs, class = c("BY", "YYYY"))
}

#' @noRd
#' @export
as_BY <- function (x) {
  x <- structure(x, class = "BY")
  return(x)
}

#'----------------------------------------------------------------------

#' @export
`[.BY` <- function(x, i) {
  as_BY(NextMethod())
}

#'----------------------------------------------------------------------

#' @noRd
vec_ptype_abbr.BY <- function (BY, ...) {
  "BY"
}
