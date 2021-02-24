timeline <- function (x) {
  #return(attr(x, "timeline"))
  YYYY_PATTERN <- "^([CRPB]Y)?([0-9]{4})$"
  matches <- stringr::str_match(x, pattern = YYYY_PATTERN)
  timeline <- unique(matches[, 2])
  stopifnot(length(timeline) == 1)
  return(timeline)
}
