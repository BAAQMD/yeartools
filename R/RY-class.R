#' @describeIn years Reporting year(s)
#' @inheritParams years
#'
#' @examples
#' # Reporting year(s)
#' RY(2015)
#' RY(2012:2016)
#'
#' @export
RY <- function (yyyy) {
  yrs <- YYYY(yyyy, prefix = "RY")
  structure(yrs, class = c("RY", "YYYY"))
}
