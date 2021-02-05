#' @export
timeline <- function (x) {
  UseMethod("timeline")
}

#' @noRd
#' @export
timeline.YYYY <- function (x) {
  return(attr(x, "timeline"))
}
