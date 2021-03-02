timeline <- function (
  x,
  pattern = "^([CRPB]Y)?([0-9]{4})$"
) {

  if (isFALSE(is.null(attr(x, "timeline")))) {
    return(attr(x, "timeline"))
  }

  matches <- stringr::str_match(x, pattern)
  prefix <- unique(na.omit(matches[, 2]))

  if (length(prefix) == 1) {
    return(prefix)
  } else if (length(prefix) > 1) {
    stop("must have only one unique prefix")
  }

  return(NULL)

}
