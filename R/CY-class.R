#' @describeIn years Calendar year(s)
#' @inheritParams years
#' @export
CY <- function (...) {
  yrs <- YYYY(..., prefix = "CY")
  structure(yrs, class = c("CY", "YYYY"))
}
