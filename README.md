
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylog

The goal of tidylog is to provide feedback about basic dplyr operations.
It provides simple wrapper functions for the most common functions, such
as `filter`, `mutate`, `select`, `full_join`, and `group_by`.

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
    group_by(cyl, mpg_round) %>%
    tally() %>%
    filter(n >= 1)
#> select: dropped 8 variables (disp, drat, wt, qsec, vs, …)
#> filter: removed 6 rows (19%)
#> mutate: new variable 'mpg_round' with 15 unique values and 0% NA
#> group_by: 17 groups (cyl, mpg_round)
#> filter: no rows removed
```

Here, it might have been accidental that the last `filter` command had
no effect.

## Installation

``` r
devtools::install_github("elbersb/tidylog")
```

## More examples

### filter & distinct

``` r
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%)
b <- filter(mtcars, mpg > 100)
#> filter: removed all rows (100%)
c <- filter(mtcars, mpg > 0)
#> filter: no rows removed
d <- filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
#> filter_at: removed 19 rows (59%)
e <- distinct(mtcars)
#> distinct: no rows removed
```

### mutate / transmute

``` r
a <- mutate(mtcars, new_var = 1)
#> mutate: new variable 'new_var' with one unique value and 0% NA
b <- mutate(mtcars, new_var = runif(n()))
#> mutate: new variable 'new_var' with 32 unique values and 0% NA
c <- mutate(mtcars, new_var = NA)
#> mutate: new variable 'new_var' with one unique value and 100% NA
d <- mutate_at(mtcars, vars(mpg, gear, drat), round)
#> mutate_at: changed 28 values (88%) of 'mpg' (0 new NA)
#> mutate_at: changed 31 values (97%) of 'drat' (0 new NA)
e <- mutate(mtcars, am_factor = as.factor(am))
#> mutate: new variable 'am_factor' with 2 unique values and 0% NA
f <- mutate(mtcars, am = as.factor(am))
#> mutate: converted 'am' from double to factor (0 new NA)
g <- mutate(mtcars, am = ifelse(am == 1, NA, am))
#> mutate: changed 13 values (41%) of 'am' (13 new NA)
h <- mutate(mtcars, am = recode(am, `0` = "zero", `1` = NA_character_))
#> mutate: converted 'am' from double to character (13 new NA)

i <- transmute(mtcars, mpg = mpg * 2, gear = gear + 1, new_var = vs + am)
#> transmute: dropped 9 variables (cyl, disp, hp, drat, wt, …)
#> transmute: changed 32 values (100%) of 'mpg' (0 new NA)
#> transmute: changed 32 values (100%) of 'gear' (0 new NA)
#> transmute: new variable 'new_var' with 3 unique values and 0% NA
```

### select

``` r
a <- select(mtcars, mpg, wt)
#> select: dropped 9 variables (cyl, disp, hp, drat, qsec, …)
b <- select(mtcars, matches("a"))
#> select: dropped 7 variables (mpg, cyl, disp, hp, wt, …)
c <- select_if(mtcars, is.character)
#> select_if: dropped all variables
```

### joins

``` r
a <- left_join(band_members, band_instruments, by = "name")
#> left_join: added 0 rows and added one column (plays)
b <- full_join(band_members, band_instruments, by = "name")
#> full_join: added one row and added one column (plays)
c <- anti_join(band_members, band_instruments, by = "name")
#> anti_join: removed 2 rows and added no new columns
```

## Turning logging off, registering additional loggers

To turn off the output for just a particular function call, you can
simply call the dplyr functions directly, e.g. `dplyr::filter`.

To turn off the output more permanently, set the global option
`tidylog.display` to an empty list:

``` r
options("tidylog.display" = list())  # turn off
a <- filter(mtcars, mpg > 20)

options("tidylog.display" = NULL)    # turn on
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%)
```

This option can also be used to register additional loggers. The option
`tidylog.display` expects a list of functions. By default (when
`tidylog.display` is set to NULL), tidylog will use the `message`
function to display the output, but if you prefer `print`, simply
overwrite the option:

``` r
options("tidylog.display" = list(print))
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%)
```

To print the output both to the screen and to a file, you could
use:

``` r
log_to_file <- function(text) cat(text, file = "log.txt", sep = "\n", append = TRUE)
options("tidylog.display" = list(message, log_to_file))
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%)
```

## Namespace conflicts

Tidylog redefines several of the functions exported by dplyr, so it
should be loaded last, otherwise there will be no output. A more
explicit way to resolve namespace conflicts is to use the
[conflicted](https://CRAN.R-project.org/package=conflicted) package:

``` r
library(dplyr)
library(tidylog)
library(conflicted)
for (f in getNamespaceExports("tidylog")) {
    conflicted::conflict_prefer(f, 'tidylog', quiet = TRUE)
}
```
