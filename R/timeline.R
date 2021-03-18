#' timeline
#'
#' Get the timeline ("RY", "CY", etc.) associated with a `YYYY` object.
#'
#' @param x [YYYY] object
#' @param pattern `x` needs to match this
#'
#' @export
timeline <- function (x) {
  UseMethod("timeline")
}

#' @rdname timeline
#'
#' @export
timeline.YYYY <- function (x) {

  pattern <- "^([CRPB]Y)?([0-9]{4})$"

  if (isFALSE(is.null(attr(x, "timeline")))) {
    return(attr(x, "timeline"))
  }

  matches <- stringr::str_match(x, pattern)
  prefix <- unique(na.omit(matches[, 2]))

  if (length(prefix) == 1) {
    return(prefix)
  } else if (length(prefix) > 1) {
    stop("must have only one unique prefix")
  }

  return(NULL)

}
