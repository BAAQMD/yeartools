#' @export
expand_years <- function (
  input_data,
  years,
  year_var = "year",
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[expand_years] ", ...)

  if (year_var %not_in% names(input_data)) {
    warn_msg <- paste0("`", year_var, "` isn't in your original data; adding it")
    msg(warn_msg)
    input_data[[year_var]] <- factor(NA, levels = unique(years))
  }

  chopped_data <-
    tidyr::chop(
      input_data,
      !!year_var)

  mutated_data <-
    mutate_at(
      chopped_data,
      vars(year_var),
      ~ map(., function (...) return(years)))

  unchopped_data <-
    tidyr::unchop(
      mutated_data,
      !!year_var)

  tidied_data <-
    unchopped_data %>%
    select(
      names(input_data))

  if (inherits(years, "CY")) {
    tidied_data <-
      mutate_at(
        tidied_data,
        vars(year_var),
        ~ CY(elide_year(.)))
  }

  return(tidied_data)

}
