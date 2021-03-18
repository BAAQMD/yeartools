#' #' @rdname YYYY
#' #' @name YYYY-addition
#' #' @title Addition and subtraction
#' #'
#' #' @note supports regression and prediction use-cases
#' #' @param e1 `YYYY` object
#' #' @param e2 `YYYY` object
#' `+.YYYY` <- function (e1, e2) {
#'   stopifnot(is.numeric(e2))
#'   new_YYYY(e1 + e2, timeline = timeline(e1))
#' }
#'
#' #' @rdname YYYY-arith
#' #' @name YYYY-subtraction
#' #' @title Addition and subtraction
#' #'
#' #' @note supports regression and prediction use-cases
#' #' @param e1 `YYYY` object
#' #' @param e2 `YYYY` object
#' `-.YYYY` <- function (e1, e2) {
#'   stopifnot(is.numeric(e2))
#'   new_YYYY(e1 + e2, timeline = timeline(e1))
#' }
