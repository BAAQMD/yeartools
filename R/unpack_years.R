#' unpack_years
#'
#' Unpack a vector of "packed years" (like RY or PY) inside a tibble
#'
#' @param input_data tabular data
#' @param year_var character or `NULL`
#' @param verbose display messages
#'
#' @note Might be slow and need optimization
#'
#' @seealso [RY()] [PY()]
#'
#' @examples
#' input_data <- data.frame(year = c("RY(2007:2009)", "RY(2001)"), ems_qty = c(111, 22))
#' unpack_years(input_data, year_var = "year")
#' unpack_years(input_data)
#'
#' @importFrom vartools find_year_var
#' @importFrom stringr str_remove_all
#' @importFrom tidyr extract unnest
#' @importFrom dplyr mutate_at mutate
#' @importFrom strtools parse_integers
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

  cleaned_data <-
    dplyr::mutate_at(
      input_data,
      vars(year_var),
      ~ stringr::str_remove_all(., "[)(]"))

  extracted_data <-
    tidyr::extract(
      cleaned_data,
      !!year_var,
      into = c(".year_prefix", ".year_digits"),
      regex = PACKED_YEARS_PATTERN,
      remove = TRUE)

  nested_data <-
    dplyr::mutate_at(
      extracted_data,
      vars(all_of(".year_digits")),
      sapply,
      FUN = function (x) unlist(strtools::parse_integers(x)))

  unnested_data <-
    tidyr::unnest(
      nested_data,
      cols = c(.year_digits))

  unpacked_data <-
    tidyr::unite(
      unnested_data,
      year,
      ".year_prefix",
      ".year_digits",
      sep = "")

  return(unpacked_data)

}
