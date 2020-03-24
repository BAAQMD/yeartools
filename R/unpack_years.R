#' unpack_years
#'
#' Unpack a vector of "packed years" (like RY or PY) inside a tibble
#'
#' @note Might be slow and need optimization
#'
#' @seealso [RY()] [PY()]
#'
#' @examples
#' input_data <- tibble(year = c("RY(2007:2009)", "RY(2001)", "PY(2003)"), ems_qty = c(111, 22, 45))
#' unpack_years(input_data, year_var = "year")
#' unpack_years(input_data)
#'
#' @export
unpack_years <- function (
  input_data,
  year_var = NULL,
  verbose = getOption("verbose")
)  {

  msg <- function (...) if(isTRUE(verbose)) message("[unpack_years] ", ...)

  if (is.null(year_var)) {
    year_var <-
      vartools::find_year_var(
        input_data,
        verbose = verbose)
  }

  msg("year_var is: ", year_var)

  PACKED_YEARS_PATTERN <-
    "^([A-Z]Y)([0-9]+:?[0-9]+?)$"

  extracted_data <-
    input_data %>%
    mutate_at(
      vars(year_var),
      ~ stringr::str_remove_all(., "[)(]")) %>%
    extract(
      !!year_var,
      into = c(".year_prefix", ".year_digits"),
      regex = PACKED_YEARS_PATTERN,
      remove = TRUE)

  nested_data <-
    mutate(
      extracted_data,
      .year_digits = map(
        .year_digits,
        ~ unlist(strtools::parse_integers(.))))

  unnested_data <-
    unnest(
      nested_data,
      cols = c(.year_digits))

  unpacked_data <-
    unnested_data %>%
    unite(
      year,
      .year_prefix,
      .year_digits,
      sep = "")

  return(unpacked_data)

}
