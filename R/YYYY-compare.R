#' @param e1 `YYYY` object or character
#' @param e2 `YYYY` object or character
#' @noRd
#' @export
`==.YYYY` <- function (e1, e2) {
  as.character(e1) == as.character(e2)
}

#' @param e1 `YYYY` object or character
#' @param e2 `YYYY` object or character
#' @noRd
#' @export
`<.YYYY` <- function (e1, e2) {
  as.character(e1) < as.character(e2)
}

#' @param e1 `YYYY` object or character
#' @param e2 `YYYY` object or character
#' @noRd
#' @export
`>.YYYY` <- function (e1, e2) {
  as.character(e1) < as.character(e2)
}

#' @param e1 `YYYY` object or character
#' @param e2 `YYYY` object or character
#' @noRd
#' @export
`<=.YYYY` <- function (e1, e2) {
  as.character(e1) <= as.character(e2)
}

#' @param e1 `YYYY` object or character
#' @param e2 `YYYY` object or character
#' @noRd
#' @export
`>=.YYYY` <- function (e1, e2) {
  as.character(e1) <= as.character(e2)
}
