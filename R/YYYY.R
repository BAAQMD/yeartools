new_YYYY <- function (
  x = character(),
  timeline = NULL,
  verbose = TRUE
) {

  vec_assert(x, ptype = character())
  #vec_assert(timeline, ptype = character())

  msg <- function (...) if(isTRUE(verbose)) message("[new_YYYY] ", ...)

  x[x == ""] <- NA_character_

  if (length(x) > 0) {
    #' Only do this if x is non-empty; otherwise the result will be
    #' a length-1 vector (it'll just be "RY" or whatever `timeline` is)
    if (isFALSE(all(str_detect(x, "^[0-9]{4}$")))) {
      stop()
    }
    if (isFALSE(is.null(timeline)) && isFALSE(is.na(timeline))) {
      x <- paste0(timeline, x)
    }
  }

  result <- vctrs::new_vctr(
    x,
    timeline = timeline,
    class = c("YYYY", "character"))

  return(result)

}

#' Typically we should call `YYYY()` rather than `new_YYYY()`.
#'
#' @export
#' @noRd
YYYY <- function(
  x = character(),
  ...,
  prefix = NULL,
  pattern = "^([CRPB]Y)?([0-9]{4})$",
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[YYYY] ", ...)

  x <- c(x, ...)

  # For a zero-length `x`, just short-circuit and invoke `new_YYYY()`;
  # don't bother trying to get fancy by parsing `x`.
  if (length(x) == 0) {
    result <- new_YYYY(x, timeline = prefix)
    return(result)
  }

  # If we got a numeric `x`, just cast it to character, and then keep going.
  if (is.numeric(x)) {
    stopifnot(all(x == round(x)))
    x <- as.character(as.integer(x))
  }

  # If we got a `YYYY` argument, then we just need to check that the timelines
  # are compatible. As of today (2021-03-02), there's no support for automatically
  # aligning different timelines, so we throw an error if they're different.
  # When the timelines are identical, we just return `x` unchanged.
  if (inherits(x, "YYYY")) {
    msg("x is a YYYY object")
    if (prefix == timeline(x)) { # FIXME: what if timeline(x) is `NULL`?
      return(x) # nothing to do
    } else {
      err_msg <- paste0("[YYYY] can't automatically align ", timeline(x), " with ", prefix)
      stop(err_msg)
    }
  }

  # If we got here, then `x` should be a character vector.
  # Now, we split `x` into two parts: `prefix` and `years`.
  # `prefix` might be nonexistent, which is OK so long as `prefix` was supplied.
  stopifnot(is.character(x))
  matches <- stringr::str_match(x, pattern)
  years <- matches[, 3]

  # If an explicit `prefix` was supplied, then we'll use that.
  if (isFALSE(is.null(prefix))) {
    # The `prefix` argument takes precedence over any prefix(es) embedded in `x`,
    # so let's warn the user, as a courtesy, if the
    if (length(prefix) > 0) {
      if (all(prefix == timeline(x))) {
        # pass; this is OK
      } else {
        warning("[YYYY] a `prefix` argument was supplied, but also `x` has some valid prefix(es); ignoring prefix(es) in `x`")
      }
    }
  }

  if (isTRUE(is.null(prefix))) {
    prefixes <- unique(matches[, 2])
    if (length(prefixes) > 1) {
      err_msg <- "[YYYY] more than one unique prefix was detected in x"
      stop(err_msg)
    } else {
      prefix <- prefixes
    }
  }

  result <- new_YYYY(x = years, timeline = prefix)
  return(result)

}

#' @export
#' @noRd
RY <- function (x = character(), ...) {
  YYYY(x, ..., prefix = "RY")
}

#' @export
#' @noRd
PY <- function (x = character(), ...) {
  YYYY(x, ..., prefix = "PY")
}

#' @export
#' @noRd
BY <- function (x = character(), ...) {
  YYYY(x, ..., prefix = "BY")
}

#' @export
#' @noRd
CY <- function (x = character(), ...) {
  YYYY(x, ..., prefix = "CY")
}
