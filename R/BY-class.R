#' @describeIn years Base year(s)
#' @inheritParams years
#' @export
BY <- function (yyyy) {
  yrs <- YYYY(yyyy, prefix = "BY")
  structure(yrs, class = c("BY", "YYYY"))
}
