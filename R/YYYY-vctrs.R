#' @importFrom vctrs vec_ptype_abbr
#' @noRd
#' @export
vec_ptype_abbr.YYYY <- function (x, ...) {
  timeline(x)
}

#' @importFrom vctrs vec_ptype2
#' @noRd
#' @export
vec_ptype2.YYYY.YYYY <- function(x, y, ...) {
  x
}

#' @importFrom vctrs vec_cast
#' @noRd
#' @export
vec_cast.YYYY.YYYY <- function(x, to, ...) {
  x
}
