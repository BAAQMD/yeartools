#' @export
expand_years <- function (
  input_data,
  years
) {

  nested <-
    input_data %>%
    mutate(
      year = lapply(1:n(), function (...) years))

  expanded <-
    unnest(nested)

  return(expanded)

}
