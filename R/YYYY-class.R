#' Years (the DataBank way)
#'
#' This family of functions helps to index requests for DataBank "files".
#'
#' @note **FIXME: more and better description.** Why do we need these? What are they for?
#'
#' @param yyyy four-digit integer(s)
#'
#' @seealso [DB_XXXX_CONCORDANCE]
#'
#' @name years
#' @rdname years
NULL

#' @noRd
YYYY <- function (..., prefix) {
  yyyy <- c(...)
  stopifnot(is.numeric(yyyy))
  repr <- paste0(prefix, yyyy)
  structure(repr, class = "YYYY")
}
