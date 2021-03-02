#' @export
#' @noRd
vec_ptype_abbr.YYYY <- function (x, ...) {
  attr(x, "timeline")
}

#' @export
#' @noRd
vec_ptype_full.YYYY <- function(x, ...) {
  timeline(x)
}

#' #' @export
#' #' @noRd
vec_ptype2.YYYY.YYYY <- function (x, y, ...) {
  if (isTRUE(timeline(x) == timeline(y))) {
    return(x)
  } else {
    stop("[vec_ptype2] incompatible timelines")
  }
}

#' @export
#' @noRd
vec_ptype2.YYYY.character <- function (x, y, ...) {
  x
}

#' @export
#' @noRd
vec_ptype2.character.YYYY <- function (x, y, ...) {
  format(y)
}

#' #' @export
#' #' @noRd
vec_cast.YYYY.YYYY <- function (x, to, ...) {
  message("[vec_cast.YYYY.YYYY]")
  if (isTRUE(timeline(x) == timeline(to))) {
    return(x)
  } else {
    stop("[vec_cast] incompatible timelines")
  }
}

#' @export
#' @noRd
vec_cast.YYYY.character <- function (x, to, ...) YYYY(x)

#' @export
#' @noRd
vec_cast.character.YYYY <- function (x, to, ...) format(x)

#' #' @export
#' #' @noRd
#' vec_c.YYYY.YYYY <- function (x, y, verbose = TRUE) {
#'   msg <- function (...) if(isTRUE(verbose)) message("[vec_c] ", ...)
#'   if (timeline(x) == timeline(y)) {
#'     msg("timeline(x) is: ", timeline(x))
#'     year <- c(elide_year(x), elide_year(y))
#'     result <- new_YYYY(year, timeline = timeline(x))
#'     msg("timeline(result) is: ", timeline(result))
#'   } else {
#'     warning("coercing to character")
#'     c(as.character(x), as.character(y))
#'   }
#'   return(result)
#' }
