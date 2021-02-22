#' #' @param x `YYYY` object
#' #' @param i integer
#' #' @noRd
#' #' @export
#' `[.YYYY` <- function(x, i) {
#'   timeline <- timeline(x)
#'   x <- as.integer(x)
#'   new_YYYY(NextMethod(), timeline = timeline)
#' }
