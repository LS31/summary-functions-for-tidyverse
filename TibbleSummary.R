library(tidyverse)

#' Summarises continous data. For grouped tibbles, summary statistics are done
# for each group seperately. By default, analyses all numeric variables. You
# can select specific variables.
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#'
#' @return A tibble with the summary.
summarise_numeric <- function(x, ...) {
  vars <- lazyeval::lazy_dots(...)
  if (!is_empty(vars)) {
    x <- select_(.data = x, .dots = vars)
  }

  if (!dplyr::is.grouped_df(x)) {
    select_if(x, is.numeric) %>%
      map_df(~ tidy(summary(.x)), .id = "variable")
  } else {
    select_if(x, is.numeric) %>%
      by_slice(
        map_df,
        ~ tidy(summary(.x)),
        .id = "variable",
        .collate = "rows")
  }
}

#' Counts observations by group (using dplyr::count), adding an extra column
#' with frequencies (rounded) within the lowest grouping level.
#'
#' @param x A tibble.
#' @param ... Variables to group by (uses dplyr::count).
#' @param sort If TRUE will sort output in descending order of n.
#' @param freq_digits Number of digits to round percentages.
#'
#' @return A tibble with counts and frequencies.
countfreq <- function(x, ..., sort = TRUE, freq_digits = 2) {
  vars <- lazyeval::lazy_dots(...)
  count_(x, vars, sort = sort) %>%
    mutate("freq" = round(n / sum(n) * 100, freq_digits))
}
