#' gather_years
#'
#' @param input_data \code{data.frame} with columns of values named 1990, 2000, etc. (four-digit years)
#' @param value_col name of column for gathered values
#' @param ...  currently ignored
#'
#' @return \code{data.frame} with columns \code{year} (int) and \code{ems_qty} (optional: change name via \code{value}) (dbl)
#' @export
#'
gather_years <- function (input_data, value_col = "ems_qty", na.rm = FALSE, ...) {

  gather_vars <-
    names(input_data) %>%
    select_vars(dplyr::matches("([A-Z]Y)?[[:digit:]]{4}"))

  gathered <-
    input_data %>%
    tidyr::gather_("year", value_col, gather_vars, convert = TRUE)

  if (isTRUE(na.rm)) {

    filter_dots <- lazyeval::interp(~ !is.na(var), var = as.name(value_col))
    filtered <- filter_(gathered, .dots = filter_dots)
    return(filtered)

  } else {

    return(gathered)

  }

}
