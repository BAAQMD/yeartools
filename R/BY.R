#' @describeIn years Base year(s)
#' @inheritParams years
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
#' @export
type_sum.BY <- function (BY, ...) {
  "BY"
}
