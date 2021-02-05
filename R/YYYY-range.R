#' @param x `YYYY` object
#' @param ... ignored
#' @noRd
min.YYYY <- function (x, na.rm = FALSE) {
  min_value <- min(as.integer(x), na.rm = na.rm)
  new_YYYY(min_value, timeline = timeline(x))
}

#' @param x `YYYY` object
#' @param ... ignored
#' @noRd
max.YYYY <- function (x, na.rm = FALSE) {
  max_value <- max(as.integer(x), na.rm = na.rm)
  new_YYYY(max_value, timeline = timeline(x))
}

#' @param x `YYYY` object
#' @param ... ignored
#'
#' @noRd
range.YYYY <- function (x, na.rm = na.rm) {
  value_range <- range(as.integer(x), na.rm = na.rm)
  new_YYYY(value_range, timeline = timeline(x))
}
