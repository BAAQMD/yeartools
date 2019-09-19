#' parse_year
#'
#' Extracts a four-digit "naive" representation from various other representations of a "year".
#'
#' @param x character, numeric, or YYYY-class
#' @param ... ignored for now
#'
#' @return four-digit integer
#'
#' @examples
#' parse_year("CY2030")
#' parse_year("RY2015")
#' parse_year("BY2011")
#' parse_year("1999")
#'
#' @export
parse_year <- function (x, ...) {
  UseMethod("parse_year", x)
}

#' @method parse_year character
parse_year.character <- function (x, ...) {
  parsed <- as.integer(readr::parse_number(x))
  is_four_digits <- (parsed >= 1000) && (parsed <= 9999)
  if (!all_true(is_four_digits)) {
    err_msg <- stringr::str_c(
      "some parsed values are not four digits")
    stop(err_msg)
  }
  return(parsed)
}

#' @method parse_year YYYY
parse_year.YYYY <- function (x, ...) {
  parsed <- parse_year.character(as.character(x))
  return(parsed)
}

#' @method parse_year default
parse_year.default <- function (x, ...) {

  if (hasMethod("as.character", class(x))) {
    parsed <- parse_year.character(as.character(x))
    return(parsed)
  }

  err_msg <- str_c("[parse_year] Don't know how to parse an object of class ", class(x))
  stop(err_msg)

}
