#' gather_years
#'
#' Gather columns beginning with `CY`, `RY`, `PY`, as well as columns that look like "naive" four-digit years.
#'
#' @param input_data tabular data
#' @param ... name of column for gathered values; defaults to `ems_qty`
#' @param pattern gather columns matching this regexp
#' @param convert as in [tidyr::gather()] (runs [type.convert()])
#' @param na.rm as in [tidyr::gather()] (drops rows from output where value would be `NA`)
#' @param verbose display messages
#'
#' @return tabular data with column `year`
#' @export
#'
gather_years <- function (
  input_data,
  ...,
  year_var = "year",
  year_pattern = "([A-Z]Y)?[[:digit:]]{4}",
  convert = TRUE,
  na.rm = TRUE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[gather_years] ", ...)

  value_var <-
    eval(substitute(alist(...)))

  if (length(value_var) == 0) {
    value_var <- "ems_qty"
    msg("value_var defaulting to: ", value_var)
  } else {
    msg("value_var is: ", value_var)
  }

  gather_vars <-
    tidyselect::vars_select(
      names(input_data),
      tidyselect::matches(year_pattern))

  msg("gather_vars is: ", str_csv(gather_vars))

  gathered_data <-
    tidyr::pivot_longer(
      input_data,
      cols = gather_vars,
      names_to = year_var,
      values_to = value_var,
      values_drop_na = na.rm)

  return(gathered_data)

}
