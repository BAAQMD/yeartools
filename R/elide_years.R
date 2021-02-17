#' elide_years
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#'
#' @param x `CY`, `BY`, `RY`, or `PY` object
#' @param ... futher arguments
#'
#' @importFrom tidyselect all_of
#'
#' @export
elide_years <- function (x, ...) {
  UseMethod("elide_years")
}

#' @rdname elide_years
#' @method elide_years default
#' @export
elide_years.default <- function (x, ...) {
  elide_year(x, ...)
}

#' @rdname elide_years
#' @method elide_years data.frame
#' @export
elide_years.data.frame <- function (x, ...) {

  if ("year" %in% names(x)) {

    result <-
      dplyr::mutate_at(
        x,
        vars(all_of("year")),
        ~ elide_year(., ...))

  } else {

    # FIXME: issue a soft warning?
    result <- x

  }

  return(result)

}
