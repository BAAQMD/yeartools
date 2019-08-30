#' @describeIn years Calendar year(s)
#' @inheritParams years
#' @export
CY <- function (yyyy) {
  yrs <- YYYY(yyyy, prefix = "CY")
  structure(yrs, class = c("CY", "YYYY"))
}
