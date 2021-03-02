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
  pattern = "([A-Z]Y)([[:digit:]]{4})",
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

  tidied_data <-
    tidyr::gather(
      input_data,
      key = !!year_var,
      value = !!value_var,
      gather_vars)

  #
  # TODO: use `pivot_longer()` instead of `gather()`
  #
  # tidied_data <-
  #   tidyr::pivot_longer(
  #     input_data,
  #     cols = gather_vars,
  #     names_to = year_var,
  #     values_to = !!value_var,
  #     values_drop_na = na.rm)

  # Try to guess the timeline: if all prefixes are the same,
  # then use that. If not, then leave it as character.
  x <- dplyr::pull(tidied_data, !!year_var)
  match_list <- stringr::str_match_all(x, pattern)
  timeline <- unique(map_chr(match_list, pluck, 2))
  if (length(timeline) == 1) {

    msg("auto-detected timeline: ", timeline)
    year <- as.integer(map_chr(match_list, pluck, 3))
    msg("first 3 years are: ", strtools::str_csv(year))
    x <- YYYY(year, timeline = timeline)
    tidied_data <- mutate(tidied_data, !!year_var := x)

  } else {

    msg("timelines (that is, prefixes) are inconsistent (", str_csv(timeline))
    msg("leaving year column (", year_var, ") as character")

  }

  return(tidied_data)

}
