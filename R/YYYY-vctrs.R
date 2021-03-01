#' @export
#' @noRd
vec_ptype_abbr.YYYY <- function (x, ...) {
  attr(x, "timeline")
}

#' @export
#' @noRd
vec_ptype2.YYYY.YYYY <- function (x, y) new_YYYY()

#' @export
#' @noRd
vec_ptype2.YYYY.character <- function (x, y, ...) character()

#' @export
#' @noRd
vec_ptype2.character.YYYY <- function (x, y, ...) character()

#' @export
#' @noRd
vec_cast.YYYY.YYYY <- function (x, to, ...) x

#' @export
#' @noRd
vec_cast.YYYY.character <- function (x, to, ...) YYYY(x)

#' @export
#' @noRd
vec_cast.character.YYYY <- function (x, to, ...) format(x)

#' @export
#' @noRd
vec_c.YYYY.YYYY <- function (x, y) {
  if (timeline(x) == timeline(y)) {
    timeline <- timeline(x)
    year <- c(as.integer(x), as.integer(y))
    result <- new_YYYY(year, timeline)
  } else {
    warning("coercing to character")
    c(as.character(x), as.character(y))
  }
  return(result)
}
