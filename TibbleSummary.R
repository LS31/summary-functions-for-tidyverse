library(tidyverse)

#' Summarises continous data. For grouped tibbles, summary statistics are done
#' for each group seperately. By default, analyses all numeric variables. You
#' can select specific variables.
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
countfreq <- function(x, ..., sort = FALSE, freq_digits = 1) {
  vars <- lazyeval::lazy_dots(...)
  count_(x, vars, sort = sort) %>%
    mutate("freq" = round(n / sum(n) * 100, freq_digits))
}

#' Counts missing observations (NA's) for all variables in a tibble, including
#' (rounded) frequencies. By default, analyses all numeric variables. You
#' can select specific variables.
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#' @param sort If TRUE will sort output in descending order of n.
#' @param freq_digits Number of digits to round percentages.
#'
#' @return A tibble with counts and frequencies.
summarise_na <- function(x,
                         ...,
                         sort = TRUE,
                         freq_digits = 1,
                         remove_nonmissing = TRUE) {
  vars <- lazyeval::lazy_dots(...)
  if (!is_empty(vars)) {
    x <- select_(.data = x, .dots = vars)
  }

  n_obs <- nrow(x)

  missingness <- summarise_all(x,
                               funs(sum(is.na(.)))) %>%
    gather(key = "variable", value = "n_missing") %>%
    mutate("freq_missing" = round(n_missing / n_obs * 100, percentage_digits))

  if (remove_nonmissing) {
    missingness <- filter(missingness, n_missing > 0)
  }

  if (sort) {
    missingness <- arrange(missingness, desc(n_missing))
  }

  missingness
}
