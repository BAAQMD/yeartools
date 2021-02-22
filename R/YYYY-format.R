#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
#' @export
format.YYYY <- function (x, ...) {
  attr(x, "timeline") <- NULL
  return(unclass(x))
}

#' @param x `YYYY` object
#' @param ... ignored
#' @noRd
#'
#' @export
print.YYYY <- function (x, ...) {
  x <- as.character(x)
  NextMethod()
}
