#' @describeIn years Base year(s)
#' @inheritParams years
#' @export
BY <- function (...) {
  yrs <- YYYY(..., prefix = "BY")
  structure(yrs, class = c("BY", "YYYY"))
}
