#' timeline
#'
#' Get the timeline ("CY", "RY", etc.) of a `YYYY` object.
#'
#' @param x [YYYY] object
#'
#' @export
timeline <- function (x) {
  UseMethod("timeline")
}

#' @noRd
#' @export
timeline.YYYY <- function (x) {
  return(attr(x, "timeline"))
}
