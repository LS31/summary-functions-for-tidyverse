#' Summarises continous data.
#'
#' For grouped tibbles, summary statistics are done for each group
#' seperately. By default, analyses all numeric variables. You can
#' select specific variables using all syntax avaiable when using dplyr::select.
#' 
#' As summarising numeric variables usually results in unreadable numbers with 
#' many decimals, quick rounding is available for your convenience. Note that 
#' you lose precision by rounding, and that all numeric values are rounded, 
#' regardless whether the summarized variable was an integer or double 
#' (e.g. all numbers get 2 decimals, regardless if that makes sense).
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#' @param digits integer indicating the number of decimal places (uses round). Use NA for no rounding.
#' @return A tibble with the summary.
#' @export
summarise_numeric <- function(x, ..., digits = NA) {
  .Deprecated(new = "skimr::skim()", 
              msg = "The package tibblesummary is no longer maintained. I would suggest migrating to a combination of the packages janitor, skimr, and ggstatsplot for better, more comprehensive functionality.")
  
  
  dot_vars <- rlang::quos(...)
  if (!rlang::is_empty(dot_vars)) {
    x <- dplyr::select(x, !!!dot_vars)
  }

  if (!dplyr::is_grouped_df(x)) {
    output <- x %>%
      dplyr::select_if(is.numeric) %>%
      purrr::map_dfr(~ broom::tidy(summary(.x)), .id = "variable") %>%
      tibble::as_tibble()
  } else {
    keep_group_vars <- groups(x)
    output <- x %>%
      dplyr::select_if(is.numeric) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        .out = purrr::map(.x = .$data,
                          .f = purrr::map_dfr,
                          ~ broom::tidy(summary(.x)),
                          .id = "variable")) %>%
      dplyr::select(-data) %>%
      tidyr::unnest() %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!!keep_group_vars)
  }
  
  if (is.numeric(digits)) {
    output <- output %>%
      dplyr::mutate_if(.predicate = is.numeric,
                       .funs = round,
                       digits = digits)
  }
  
  output
}

#' Summarises dates. 
#' 
#' For grouped tibbles, summary statistics are done
#' for each group seperately. By default, analyses all numeric variables. You
#' can select specific variables using all syntax avaiable when using dplyr::select.
#'
#' This selects all variables that are lubridate::is.instant().
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#'
#' @return A tibble with the summary.
#' @export
summarise_date <- function(x, ...) {
  .Deprecated(new = "skimr::skim()", 
              msg = "The package tibblesummary is no longer maintained. I would suggest migrating to a combination of the packages janitor, skimr, and ggstatsplot for better, more comprehensive functionality.")
  
  dot_vars <- rlang::quos(...)
  if (!rlang::is_empty(dot_vars)) {
    x <- dplyr::select(x, !!!dot_vars)
  }

  x <- dplyr::mutate_if(x, lubridate::is.instant, as.POSIXct, origin = "1970-01-01", tz = "UTC")

  if (!dplyr::is_grouped_df(x)) {
    dplyr::select_if(x, lubridate::is.instant) %>%
      purrr::map_dfr(~ broom::tidy(summary(.x)),
                     .id = "variable") %>%
      dplyr::mutate_at(dplyr::vars(-variable), as.POSIXct, origin = "1970-01-01", tz = "UTC") %>%
      tibble::as_tibble()
  } else {
    keep_group_vars <- groups(x)
    dplyr::select_if(x, lubridate::is.instant) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        .out = purrr::map(.x = .$data,
                          .f = purrr::map_dfr,
                          ~ broom::tidy(summary(.x)),
                          .id = "variable")) %>%
      dplyr::select(-data) %>%
      tidyr::unnest() %>%
      dplyr::mutate_if(is.double, as.POSIXct, origin = "1970-01-01", tz = "UTC") %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!!keep_group_vars)
  }
}

#' Counts observations by group including frequencies.
#'
#' This is based on dplyr::count, adding an extra column
#' with frequencies (rounded) within the lowest grouping level.
#'
#' @param x A tibble.
#' @param ... Variables to group by (uses dplyr::count).
#' @param sort If TRUE will sort output in descending order of n.
#' @param freq_digits Number of digits to round percentages.
#' @return A tibble with counts and frequencies.
#' @export
countfreq <- function(x, ...,
                      sort = FALSE,
                      freq_digits = 1) {
  .Deprecated(new = "janitor::tabyl()", 
              msg = "The package tibblesummary is no longer maintained. I would suggest migrating to a combination of the packages janitor, skimr, and ggstatsplot for better, more comprehensive functionality.")
  
  
  dplyr::count(x, ..., sort = sort) %>%
    dplyr::mutate(freq = round(n / sum(n) * 100, freq_digits))
}

#' Counts missing observations (NA's)
#'
#' This includes frequencies. By default, analyses all variables.
#' You can select specific variables using all syntax avaiable when using dplyr::select.
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#' @param sort If TRUE will sort output in descending order of n.
#' @param freq_digits Number of digits to round percentages.
#' @param remove_nonmissing If TRUE remove variables without any missing values.
#' @return A tibble with counts and frequencies.
#' @export
summarise_na <- function(x,
                         ...,
                         sort = TRUE,
                         freq_digits = 1,
                         remove_nonmissing = TRUE) {
  .Deprecated(new = "skimr::skim()", 
              msg = "The package tibblesummary is no longer maintained. I would suggest migrating to a combination of the packages janitor, skimr, and ggstatsplot for better, more comprehensive functionality.")
  
  dot_vars <- rlang::quos(...)
  if (!rlang::is_empty(dot_vars)) {
    x <- dplyr::select(x, !!!dot_vars)
  }

  if (!dplyr::is_grouped_df(x)) {
    n_obs <- nrow(x)
    
    missingness <- 
      dplyr::summarise_all(x, dplyr::funs(sum(is.na(.)))) %>%
      tidyr::gather(key = variable, value = n_missing) %>%
      dplyr::mutate(freq_missing = round(n_missing / n_obs * 100, freq_digits)) %>%
      tibble::as_tibble()
    
    
    if (sort) {
      missingness <- dplyr::arrange(missingness, dplyr::desc(n_missing))
    }
  } else {
    keep_group_vars <- groups(x)
    
    missingness <- tidyr::nest(x) %>%
      dplyr::mutate(
        .out = purrr::map(.x = .$data,
                          .f = summarise_na)) %>%
      dplyr::select(-data) %>%
      tidyr::unnest() %>%
      tibble::as_tibble() %>%
      dplyr::group_by(!!!keep_group_vars)
    
    if (sort) {
      missingness <- dplyr::arrange(missingness, 
                                    dplyr::desc(n_missing),
                                    .by_group = TRUE)
    }
  }
    
  if (remove_nonmissing) {
    missingness <- dplyr::filter(missingness, n_missing > 0)
  }
  
  missingness
}
