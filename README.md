
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylog

The goal of tidylog is to provide feedback about basic dplyr operations.
It provides simple wrapper functions for the most common functions, such
as `filter`, `mutate`, `select`, and `group_by`.

## Example

Load `tidylog` after `dplyr`:

``` r
library("dplyr")
library("tidylog", warn.conflicts = FALSE)
```

Tidylog will give you feedback, for instance when filtering a data
frame:

``` r
filtered <- filter(mtcars, cyl == 4)
#> filter: removed 21 rows (66%)
```

This can be especially helpful in longer pipes:

``` r
summary <- mtcars %>%
    select(mpg, cyl, hp) %>%
    filter(mpg > 15) %>%
    mutate(mpg_round = round(mpg)) %>%
    mutate(mpg_round = ifelse(mpg_round > 30, NA, mpg_round)) %>%
    filter(!is.na(mpg_round)) %>%
    group_by(cyl, mpg_round) %>%
    tally() %>%
    filter(n >= 1)
#> select: dropped 8 variables (disp, drat, wt, qsec, vs, …) 
#> filter: removed 6 rows (19%) 
#> mutate: new variable 'mpg_round' with 15 unique values and 0% NA 
#> mutate: changed 2 values (8%) of 'mpg_round' (2 new NA) 
#> filter: removed 2 rows (8%) 
#> group_by: 15 groups [cyl, mpg_round] 
#> filter: no rows removed
```

Here, it might have been accidental that the last `filter` command had
no effect.

## Installation

``` r
devtools::install_github("elbersb/tidylog")
```
