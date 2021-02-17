#' @param x `YYYY` object
#' @param i integer
#' @noRd
#' @export
`[.YYYY` <- function(x, i) {
  new_YYYY(NextMethod(), timeline = timeline(x))
}
