#' #' @param x `YYYY` object
#' #' @param ... ignored
#' #'
#' #' @noRd
#' #' @export
#' format.YYYY <- function (x, ...) {
#'   attr(x, "timeline") <- NULL
#'   return(unclass(x))
#' }
#'
#' #' @param x `YYYY` object
#' #' @param ... ignored
#' #' @noRd
#' #'
#' #' @export
#' print.YYYY <- function (x, ...) {
#'   x <- as.character(x)
#'   NextMethod()
#' }

#' #' @export
#' format.YYYY <- function(x, ...) {
#'   x_valid <- which(!is.na(x))
#'   year <- field(x, "year")[x_valid]
#'   ret <- rep(NA_character_, vec_size(x))
#'   ret[x_valid] <- paste0(timeline(x), format(year))
#'   return(ret)
#' }

#' @export
format.YYYY <- function (x, ...) {
  attr(x, "timeline") <- NULL
  format(unclass(x))
}
