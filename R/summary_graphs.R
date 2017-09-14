#' Creates a ggplot histogram for multiple numeric variables.
#'
#' By default, analyses all numeric variables. You can select
#' specific variables using all syntax avaiable when using dplyr::select.
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#' @return A ggplot.
#' @export
multihistogram_numeric <- function(x, ...) {
  dot_vars <- rlang::quos(...)
    if (!rlang::is_empty(dot_vars)) {
      x <- dplyr::select(x, !!!dot_vars)
  }

  x %>%
    dplyr::select_if(is.numeric) %>%
    tidyr::gather() %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::facet_wrap(~ key, scales = "free") +
    ggplot2::geom_histogram()
}

#' Creates a ggplot histogram for multiple Date variables.
#'
#' By default, analyses all variables conforming to lubridate::is.instant. You can select specific variables using all syntax avaiable when using dplyr::select.
#'
#' @param x A tibble.
#' @param ... Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)
#' @return A ggplot.
#' @export
multihistogram_date <- function(x, ...) {
  dot_vars <- rlang::quos(...)
  if (!rlang::is_empty(dot_vars)) {
    x <- dplyr::select(x, !!!dot_vars)
  }

  x <- dplyr::mutate_if(x, lubridate::is.instant, as.POSIXct, origin = "1970-01-01", tz = "UTC")

  x %>%
    purrr::keep(lubridate::is.instant) %>%
    tidyr::gather() %>%
    ggplot2::ggplot(ggplot2::aes(value)) +
    ggplot2::facet_wrap(~ key, scales = "free") +
    ggplot2::geom_histogram()
}
