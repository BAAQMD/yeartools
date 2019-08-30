#' @export
parse_year <- function (x) {

  if (is.character(x)) {
    yyyy <- parse_number(x)
  } else if (inherits(x, "YYYY")) {
    yyyy <- as.integer(x)
  } else if (hasMethod("as.integer", class(x))) {
    yyyy <- as.integer(x)
  } else {
    msg <- str_c("[parse_year] Don't know how to parse ", as.character(x))
    stop(msg)
  }

  return(yyyy)

}
