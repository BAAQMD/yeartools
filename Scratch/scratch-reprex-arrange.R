library(tidyverse)
library(vctrs)

# Example data.
df1 <-
  mtcars %>%
  as_tibble(rownames = "name") %>%
  separate(name, into = c("make", "model"), fill = "right", extra = "merge") %>%
  filter(make %in% c("Merc", "Fiat"))

# Nothing fancy here. Yields correct row ordering.
arrange(df1, make, mpg)

#
# Now turn the `make` column into a rudimentary `vctr`.
# This yields an **incorrect** row ordering:
#
# - `make` is correctly ordered; but
# - within each make, `mpg` is not.
#
df2 <- mutate(df1, make = new_vctr(make, class = c("foo", "character")))
arrange(df2, make, mpg)

#
# Implement a method to support arranging "as character".
# Yields a correct row ordering again.
#
vec_cast.character.foo <- function (x, to, ...) format(x)
arrange(df2, as.character(make), mpg)
