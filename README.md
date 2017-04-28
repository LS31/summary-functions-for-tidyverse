# Summary functions for the tidyverse
These are a few simple functions combining the power of dplyr, purrr, and broom to quickly create summary statistics for multiple variables of a tibble or data frame.

* _summarise_numeric_ summarises continous data. For grouped tibbles, summary statistics are calculated for each group seperately. You can select specific variables (like in dplyr), or select no specific variables at all, in which case all numeric variables are analyzed.

* _countfreq_ is an extension to dplyr's count. It counts observations by group, adding an extra column with (rounded) frequencies within the lowest grouping level.
