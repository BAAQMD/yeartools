#' @export
expand_years <- function (
  input_data,
  years
) {

  f <- function (...) return(years)

  nested <-
    dplyr::mutate_at(
      input_data,
      vars(year),
      ~ purrr::map(., f))

  expanded <-
    tidyr::unnest(
      nested,
      cols = c(year))

  return(expanded)

}
