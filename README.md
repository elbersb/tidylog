
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylog

The goal of tidylog is to provide feedback about basic dplyr operations.
It provides simple wrapper functions for the most common functions, such
as `filter`, `mutate`, `select`, and `group_by`.

## Example

Load `tidylog` after `dplyr`:

``` r
library("dplyr")
library("tidylog")
```

Tidylog will give you feedback, for instance when filtering a data
frame:

``` r
subset1 <- filter(mtcars, cyl == 4)
#> filter: removed 21 rows (66%)
# accidentically filtered nothing:
subset2 <- filter(mtcars, mpg > 10) 
#> filter: no rows removed
```

This can be especially helpful in longer pipes:

``` r
summary <- mtcars %>%
    select(mpg, cyl, hp) %>%
    filter(mpg > 15) %>%
    mutate(mpg_round = round(mpg)) %>%
    group_by(cyl, mpg_round) %>%
    tally()
#> select: dropped 8 variables (disp, drat, wt, qsec, vs, …) 
#> filter: removed 6 rows (19%) 
#> mutate: new variable 'mpg_round' with 15 unique values (0% NA) 
#> group_by: 17 groups [cyl, mpg_round]
```

## Installation

``` r
devtools::install_github("elbersb/tidylog")
```
