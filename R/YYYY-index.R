#' @param x `YYYY` object
#' @param i integer
#' @export
`[.YYYY` <- function(x, i) {
  new_YYYY(NextMethod(), timeline = timeline(x))
}
