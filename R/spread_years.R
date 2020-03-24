#' spread_years
#'
#' Spread `year` into columns.
#'
#' @param input_data (tabular data)
#' @param value_var (character or symbol) defaults to whatever is returned by [find_qty_var()].
#' @param key_var (character) defaults to whatever is found by [find_year_var()].
#' @param transform (formula or function) applied to resulting column using `mutate_at()`
#' @param ... (dots) passed to [tidyr::spread()]
#' @param verbose (logical) display messages
#'
#' @return tabular data with column `year`
#' @export
#'
spread_years <- function (
  input_data,
  value_var,
  year_var,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[spread_years] ", ...)

  if (missing(value_var)) {
    value_var <-
      vartools::find_qty_var(
        input_data,
        verbose = verbose)
  }

  value_var <- rlang::enquo(value_var)
  msg("value_var is: ", value_var)

  if (missing(year_var)) {
    year_var <-
      vartools::find_year_var(
        input_data,
        verbose = verbose)
  }

  year_var <- rlang::enquo(year_var)
  msg("year_var is: ", year_var)

  spread_data <-
    tidyr::spread(
      input_data,
      key = !!year_var,
      value = !!value_var,
      ...)

  return(spread_data)

}
