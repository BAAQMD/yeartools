#'----------------------------------------------------------------------
#'
#' vec_ptype_abbr() and vec_ptype_full()
#'
#'----------------------------------------------------------------------

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

#'----------------------------------------------------------------------
#'
#' vec_ptype2()
#'
#'----------------------------------------------------------------------

#' #' @export
#' #' @noRd
vec_ptype2.YYYY.YYYY <- function (x, y, ...) {
  if (isTRUE(timeline(x) == timeline(y))) {
    return(x)
  } else {
    stop_incompatible_type(
      message = "timelines are incompatible")
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
  y
}

#' @export
#' @noRd
vec_ptype2.YYYY.double <- function (x, y, ...) {
  x
}
#' @export
#' @noRd
vec_ptype2.double.YYYY <- function (x, y, ...) {
  y
}

#'----------------------------------------------------------------------
#'
#' vec_cast()
#'
#'----------------------------------------------------------------------

#' @export
#' @noRd
vec_cast.YYYY.YYYY <- function (x, to, ...) {
  #message("[vec_cast.YYYY.YYYY]")
  #message("x is: ", x)
  if (isTRUE(identical(timeline(x), timeline(to))) && isFALSE(is.na(timeline(x))) && isFALSE(is.na(timeline(to)))) {
    # pass; timelines are identical
  } else {
    stop_incompatible_cast(
      x = x, to = to, x_arg = "x", to_arg = "to",
      message = "[vec_cast] incompatible timelines")
  }
  return(x)
}

#' @export
#' @noRd
vec_cast.YYYY.character <- function (x, to, ...) {
  #message("[vec_cast.YYYY.character] str(x) is: ", str(x))
  result <- YYYY(x)
  #message("[vec_cast.YYYY.character] str(result) is: ", str(result))
  #message("[vec_cast.YYYY.character] timeline(result) is: ", timeline(result))
  return(result)
}

#' @export
#' @noRd
vec_cast.character.YYYY <- function (x, to, ...) {
  #message("[vec_cast.character.YYYY]")
  result <- format(x)
  return(result)
}

#' @export
#' @noRd
vec_cast.YYYY.double <- function (x, to, ...) {
  err_msg <- paste(
    "Numbers can't be directly compared or promoted to CY, BY, RY, or PY objects.",
    "Use CY(), BY(), RY(), or PY() to promote doubles to YYYY objects.",
    "Use elide_year() or parse_year() to drop timelines from YYYY objects.",
    "See examples in `?elide_year`, `?parse_year`, and `?YYYY`.")
  stop_incompatible_type(
    x, to, x_arg = "x", y_arg = "to",
    message = err_msg)
}

#' @export
#' @noRd
vec_cast.double.YYYY <- function (x, to, ...) {
  err_msg <- paste(
    "Numbers can't be directly compared or promoted to CY, BY, RY, or PY objects.",
    "Use CY(), BY(), RY(), or PY() to promote doubles to YYYY objects.",
    "Use elide_year() or parse_year() to drop timelines from YYYY objects.",
    "See examples in `?elide_year`, `?parse_year`, and `?YYYY`.")
  stop_incompatible_type(
    x, to, x_arg = "x", y_arg = "to",
    message = err_msg)
}

#'----------------------------------------------------------------------
#'
#' vec_c()
#'
#'----------------------------------------------------------------------

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
