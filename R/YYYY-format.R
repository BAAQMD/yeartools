#' @export
format.YYYY <- function (x, ...) {
  #out <- paste0(timeline(x), x)
  out <- format(unclass(x))
  out[is.na(x)] <- NA_character_
  return(out)
}
