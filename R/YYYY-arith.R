#' @note supports regression and prediction use-cases
#' @param e1 `YYYY` object
#' @param e2 `YYYY` object
#' @noRd
`+.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  new_YYYY(e1 + e2, timeline = timeline(e1))
}

#' @note supports regression and prediction use-cases
#' @param e1 `YYYY` object
#' @param e2 `YYYY` object
#' @noRd
`-.YYYY` <- function (e1, e2) {
  stopifnot(is.numeric(e2))
  new_YYYY(e1 + e2, timeline = timeline(e1))
}
