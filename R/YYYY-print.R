#' @export
print.YYYY <- function (x, ...) {
  cat("<", attr(x, "timeline"), ">", sep = "")
  x <- field(x, "year")
  NextMethod()
}
