#' @export
expand_years <- function (
  input_data,
  years,
  year_var = "year",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[expand_years] ", ...)

  f <- function (...) return(years)

  if (year_var %not_in% names(input_data)) {
    warn_msg <- paste0("`", year_var, "` isn't in your original data; adding it")
    msg(warn_msg)
    input_data$year <- NA_integer_
  }

  nested <-
    dplyr::mutate_at(
      input_data,
      vars(year_var),
      ~ purrr::map(., f))

  expanded <-
    tidyr::unnest(
      nested,
      cols = c(year_var))

  return(expanded)

}
