#' gather_years
#'
#' Gather columns beginning with `CY`, `RY`, `PY`, as well as columns that look like "naive" four-digit years.
#'
#' @param input_data (tabular data)
#' @param value_var (character or symbol) defaults to "ems_qty"
#' @param pattern (regexp) gather columns matching this
#' @param year_var (character)
#' @param na.rm (logical) drop rows from output where value would be `NA`
#' @param verbose (logical) display messages
#'
#' @importFrom dplyr pull
#' @importFrom rlang as_name `:=`
#' @importFrom purrr map_chr pluck
#' @importFrom strtools str_csv
#' @importFrom stringr str_match_all
#'
#' @return tabular data with column `year`
#' @export
#'
gather_years <- function (
  input_data,
  value_var,
  year_var = "year",
  pattern = "([CBRP]Y)?([0-9]{4})",
  na.rm = TRUE,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[gather_years] ", ...)

  year_var <- rlang::as_name(year_var)
  msg("year_var is: ", year_var)

  if (missing(value_var)) {

    value_var <- "ems_qty"
    warning("Please supply an explicit `value_var` --- otherwise, in the future, your code may break!")
    msg("value_var defaulting to: ", value_var)

  }

  value_var <- rlang::enquo(value_var)
  msg("value_var is: ", value_var)

  gather_vars <-
    purrr::keep(
      names(input_data),
      ~ stringr::str_detect(., pattern))

  msg("gathering: ", strtools::str_csv(gather_vars))

  gathered_data <-
    tidyr::gather(
      input_data,
      key = !!year_var,
      value = !!value_var,
      gather_vars)

  tidied_data <-
    mutate(
      gathered_data,
      across(all_of(year_var), YYYY))

  return(tidied_data)

}
