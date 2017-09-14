# Summary functions for the tidyverse
These are a few simple functions combining the power of dplyr, purrr, and broom to quickly create summary statistics for multiple variables of a tibble or data frame, using the standard selectors in dplyr (including `contains("mysubstring")` and `starts_with("myprefix")`, for example).

* `summarise_numeric` summarises continous data. For grouped tibbles, summary statistics are calculated for each group seperately. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all numeric variables are analyzed.

* `summarise_date` summarises Date objects. For grouped tibbles, summary statistics are calculated for each group seperately. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all Date variables are analyzed.

* `countfreq` is an extension to `dplyr::count`. It counts observations by group, adding an extra column with (rounded) frequencies within the lowest grouping level.

* `summarise_na` counts missing observations (_NA_'s) for all variables in a tibble, and the report includes (rounded) frequencies. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all numeric variables are analyzed.

* `multihistogram_numeric` is a quick way to create histograms for multiple numeric variables in a tibble. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all numeric variables are analyzed. It uses `ggplot`'s facets to create a quick grid of multiple histograms.

* `multihistogram_date` is a quick way to create histograms for multiple Date variables in a tibble. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all Date variables are analyzed. It uses `ggplot`'s facets to create a quick grid of multiple histograms.

# How to install

Tibblesummary is not (yet) available on CRAN. Install devtools. Then use devtools to install tibblesummary directly from GitHub.

```{r}
install.packages("devtools")
devtools::install_github("lcs31/tibblesummary")
```
# How to update

```{r}
devtools::install_github("lcs31/tibblesummary")
```
