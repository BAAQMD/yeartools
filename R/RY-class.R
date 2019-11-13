#' @describeIn years Reporting year(s)
#' @inheritParams years
#'
#' @examples
#' # Reporting year(s)
#' RY(2015)
#' RY(2012:2016)
#'
#' @export
RY <- function (...) {
  yrs <- YYYY(..., prefix = "RY")
  structure(yrs, class = c("RY", "YYYY"))
}
