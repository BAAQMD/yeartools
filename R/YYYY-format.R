#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
#' @export
format.YYYY <- function (x, ...) {
  sprintf("%s%d", timeline(x), x)
}

#' @param x `YYYY` object
#' @param ...
#' @noRd
print.YYYY <- function (x, ...) {
  x <- as.character(x)
  NextMethod()
}
