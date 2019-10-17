#' elide_years
#'
#' To elide distinctions between "CY", "BY", "RY", etc.
#'
#' @export
elide_years <- function (x, ...) {
  UseMethod("elide_years")
}

#' elide_years
#'
#' @method elide_years default
elide_years.default <- function (x, ...) {
  elide_year(x, ...)
}

#' elide_years
#'
#' @method elide_years data.frame
elide_years.data.frame <- function (x, ...) {

  if ("year" %in% names(x)) {

    result <-
      dplyr::mutate_at(
        x,
        vars(year),
        ~ elide_year(., ...))

  } else {

    # FIXME: issue a soft warning?
    result <- x

  }

  return(result)

}
