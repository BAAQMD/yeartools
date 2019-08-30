#' Unpack a vector of "packed years" (like RY or PY)
#'
#' @note Might be slow and need optimization
#'
#' @importFrom strtools unpack_integers
#' @seealso [RY()] [PY()]
#'
#' @examples
#' input_data <- tibble(years = c("RY(2007:2009)", "RY(2001)", "PY(2003)"), ems_qty = c(111, 22, 45))
#' unpack_years(input_data, year_var = "years")
#' unpack_years(input_data)
#'
#' @export
unpack_years <- function (input_data, year_var = NULL, verbose = getOption("verbose"))  {

  msg <- function (...) if(isTRUE(verbose)) message("[unpack_years] ", ...)

  if (is.null(year_var)) {
    year_var <- vartools::find_var(input_data, suffix = "year(s?)")
  }

  msg("unpacking ", year_var)

  unpacked <- local({

    nested <- local({
      packed <- pull(input_data, !!year_var)
      cleaned <- str_remove_all(packed, "[)(]")
      pattern <- "^([A-Z]Y)([0-9]+:?[0-9]+?)$"
      mtx <- str_match(cleaned, pattern)
      df <- as_tibble(mtx)
      set_names(df, c(year_var, ".fun", ".rng"))
    })

    g <- function (.fun, .rng) { f <- get(.fun); x <- unlist(parse_integers(.rng)); f(x) }
    nested[[year_var]] <- with(nested, map2(.fun, .rng, compose(as.character, g)))

    recombined <- bind_cols(select(input_data, -one_of(year_var)), nested)
    unnested <- unnest_(recombined, year_var)
    select(unnested, -one_of(".fun", ".rng"))

  })

  singular_form <- str_replace(year_var, "years$", "year")
  renamed <- rename_(unpacked, .dots = set_names(year_var, singular_form))

  return(renamed)

}
