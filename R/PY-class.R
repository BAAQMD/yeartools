#' @describeIn years Permit year(s)
#' @inheritParams years
#'
#' @examples
#' # The constructors `RY()`, `PY()`, etc. all yield instances of a very simple "S3" class (`YYYY`).
#' PY(2011:2014) %>% inherits("YYYY")
#' PY(2011:2014) %>% as.character()
#' PY(2011:2014) %>% as.integer()
#' PY(2011:2014) - 1 # can add and subtract
#' PY(2011:2014) %>% max()
#'
#' @export
PY <- function (yyyy) {
  yrs <- YYYY(yyyy, prefix = "PY")
  structure(yrs, class = c("PY", "YYYY"))
}
