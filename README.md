# Summary functions for the tidyverse
These are a few simple functions combining the power of dplyr, purrr, and broom to quickly create summary statistics for multiple variables of a tibble or data frame.

* `summarise_numeric` summarises continous data. For grouped tibbles, summary statistics are calculated for each group seperately. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all numeric variables are analyzed.

* `summarise_date` summarises Date objects. For grouped tibbles, summary statistics are calculated for each group seperately. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all Date variables are analyzed.

* `countfreq` is an extension to `dplyr::count`. It counts observations by group, adding an extra column with (rounded) frequencies within the lowest grouping level.

* `summarise_na` counts missing observations (_NA_'s) for all variables in a tibble, and the report includes (rounded) frequencies. You can select specific variables (like in `dplyr::select`), or select no specific variables at all, in which case all numeric variables are analyzed.
