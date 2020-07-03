
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidylog

[![CRAN
Version](https://www.r-pkg.org/badges/version/tidylog)](https://CRAN.R-project.org/package=tidylog)
[![Downloads](http://cranlogs.r-pkg.org/badges/tidylog)](https://CRAN.R-project.org/package=tidylog)
[![Build
Status](https://travis-ci.org/elbersb/tidylog.svg?branch=master)](https://travis-ci.org/elbersb/tidylog)
[![Coverage
status](https://codecov.io/gh/elbersb/tidylog/branch/master/graph/badge.svg)](https://codecov.io/github/elbersb/tidylog?branch=master)

The goal of tidylog is to provide feedback about dplyr and tidyr
operations. It provides simple wrapper functions for almost all dplyr
and tidyr functions, such as `filter`, `mutate`, `select`, `full_join`,
and `group_by`.

## Example

Load `tidylog` after `dplyr` and/or `tidyr`:

``` r
library("dplyr")
library("tidyr")
library("tidylog", warn.conflicts = FALSE)
```

Tidylog will give you feedback, for instance when filtering a data frame
or adding a new variable:

``` r
filtered <- filter(mtcars, cyl == 4)
#> filter: removed 21 rows (66%), 11 rows remaining
mutated <- mutate(mtcars, new_var = wt ** 2)
#> mutate: new variable 'new_var' (double) with 29 unique values and 0% NA
```

Tidylog reports detailed information for joins:

``` r
joined <- left_join(nycflights13::flights, nycflights13::weather,
    by = c("year", "month", "day", "origin", "hour", "time_hour"))
#> left_join: added 9 columns (temp, dewp, humid, wind_dir, wind_speed, …)
#>            > rows only in x     1,556
#>            > rows only in y  (  6,737)
#>            > matched rows     335,220
#>            >                 =========
#>            > rows total       336,776
```

In this case, we see that 1,556 rows from the `flights` dataset do not
have weather information.

Tidylog can be especially helpful in longer pipes:

``` r
summary <- mtcars %>%
    select(mpg, cyl, hp, am) %>%
    filter(mpg > 15) %>%
    mutate(mpg_round = round(mpg)) %>%
    group_by(cyl, mpg_round, am) %>%
    tally() %>%
    filter(n >= 1)
#> select: dropped 7 variables (disp, drat, wt, qsec, vs, …)
#> filter: removed 6 rows (19%), 26 rows remaining
#> mutate: new variable 'mpg_round' (double) with 15 unique values and 0% NA
#> group_by: 3 grouping variables (cyl, mpg_round, am)
#> tally: now 20 rows and 4 columns, 2 group variables remaining (cyl, mpg_round)
#> filter (grouped): no rows removed
```

Here, it might have been accidental that the last `filter` command had
no effect.

## Installation

Download from CRAN:

``` r
install.packages("tidylog")
```

Or install the development version:

``` r
devtools::install_github("elbersb/tidylog")
```

## Benchmarks

Tidylog will add a small overhead to each function call. This can be
relevant for very large datasets and especially for joins. If you want
to switch off tidylog for a single long-running command, simply prefix
`dplyr::` or `tidyr::`, such as in `dplyr::left_join`. See [this
vignette](https://cran.r-project.org/web/packages/tidylog/vignettes/benchmarks.html)
for more information.

## More examples

### filter, distinct, drop\_na

``` r
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%), 14 rows remaining
b <- filter(mtcars, mpg > 100)
#> filter: removed all rows (100%)
c <- filter(mtcars, mpg > 0)
#> filter: no rows removed
d <- filter_at(mtcars, vars(starts_with("d")), any_vars((. %% 2) == 0))
#> filter_at: removed 19 rows (59%), 13 rows remaining
e <- distinct(mtcars)
#> distinct: no rows removed
f <- distinct_at(mtcars, vars(vs:carb))
#> distinct_at: removed 18 rows (56%), 14 rows remaining
g <- top_n(mtcars, 2, am)
#> top_n: removed 19 rows (59%), 13 rows remaining
i <- sample_frac(mtcars, 0.5)
#> sample_frac: removed 16 rows (50%), 16 rows remaining

j <- drop_na(airquality)
#> drop_na: removed 42 rows (27%), 111 rows remaining
k <- drop_na(airquality, Ozone)
#> drop_na: removed 37 rows (24%), 116 rows remaining
```

### mutate, transmute, replace\_na, fill

``` r
a <- mutate(mtcars, new_var = 1)
#> mutate: new variable 'new_var' (double) with one unique value and 0% NA
b <- mutate(mtcars, new_var = runif(n()))
#> mutate: new variable 'new_var' (double) with 32 unique values and 0% NA
c <- mutate(mtcars, new_var = NA)
#> mutate: new variable 'new_var' (logical) with one unique value and 100% NA
d <- mutate_at(mtcars, vars(mpg, gear, drat), round)
#> mutate_at: changed 28 values (88%) of 'mpg' (0 new NA)
#>            changed 31 values (97%) of 'drat' (0 new NA)
e <- mutate(mtcars, am_factor = as.factor(am))
#> mutate: new variable 'am_factor' (factor) with 2 unique values and 0% NA
f <- mutate(mtcars, am = as.ordered(am))
#> mutate: converted 'am' from double to ordered factor (0 new NA)
g <- mutate(mtcars, am = ifelse(am == 1, NA, am))
#> mutate: changed 13 values (41%) of 'am' (13 new NA)
h <- mutate(mtcars, am = recode(am, `0` = "zero", `1` = NA_character_))
#> mutate: converted 'am' from double to character (13 new NA)

i <- transmute(mtcars, mpg = mpg * 2, gear = gear + 1, new_var = vs + am)
#> transmute: dropped 9 variables (cyl, disp, hp, drat, wt, …)
#>            changed 32 values (100%) of 'mpg' (0 new NA)
#>            changed 32 values (100%) of 'gear' (0 new NA)
#>            new variable 'new_var' (double) with 3 unique values and 0% NA

j <- replace_na(airquality, list(Solar.R = 1))
#> replace_na: converted 'Solar.R' from integer to double (7 fewer NA)
k <- fill(airquality, Ozone)
#> fill: changed 37 values (24%) of 'Ozone' (37 fewer NA)
```

### joins

For joins, tidylog provides more detailed information. For any join,
tidylog will show the number of rows that are only present in x (the
first dataframe), only present in y (the second dataframe), and rows
that have been matched. Numbers in parentheses indicate that these rows
are not included in the result. Tidylog will also indicate whether any
rows were duplicated (which is often unintentional):

``` r
x <- tibble(a = 1:2)
y <- tibble(a = c(1, 1, 2), b = 1:3) # 1 is duplicated
j <- left_join(x, y, by = "a")
#> left_join: added one column (b)
#>            > rows only in x   0
#>            > rows only in y  (0)
#>            > matched rows     3    (includes duplicates)
#>            >                 ===
#>            > rows total       3
```

More examples:

``` r
a <- left_join(band_members, band_instruments, by = "name")
#> left_join: added one column (plays)
#>            > rows only in x   1
#>            > rows only in y  (1)
#>            > matched rows     2
#>            >                 ===
#>            > rows total       3
b <- full_join(band_members, band_instruments, by = "name")
#> full_join: added one column (plays)
#>            > rows only in x   1
#>            > rows only in y   1
#>            > matched rows     2
#>            >                 ===
#>            > rows total       4
c <- anti_join(band_members, band_instruments, by = "name")
#> anti_join: added no columns
#>            > rows only in x   1
#>            > rows only in y  (1)
#>            > matched rows    (2)
#>            >                 ===
#>            > rows total       1
```

Because tidylog needs to perform two additional joins behind the scenes
to report this information, the overhead will be larger than for the
other tidylog functions (especially with large datasets).

### select, relocate, rename

``` r
a <- select(mtcars, mpg, wt)
#> select: dropped 9 variables (cyl, disp, hp, drat, qsec, …)
b <- select_if(mtcars, is.character)
#> select_if: dropped all variables
c <- relocate(mtcars, hp)
#> relocate: columns reordered (hp, mpg, cyl, disp, drat, …)
d <- select(mtcars, a = wt, b = mpg)
#> select: renamed 2 variables (a, b) and dropped 9 variables

e <- rename(mtcars, miles_per_gallon = mpg)
#> rename: renamed one variable (miles_per_gallon)
f <- rename_with(mtcars, toupper)
#> rename_with: renamed 11 variables (MPG, CYL, DISP, HP, DRAT, …)
```

### summarize

``` r
a <- mtcars %>%
    group_by(cyl, carb) %>%
    summarize(total_weight = sum(wt))
#> group_by: 2 grouping variables (cyl, carb)
#> summarize: now 9 rows and 3 columns, one group variable remaining (cyl)

b <- iris %>%
    group_by(Species) %>%
    summarize_all(list(min, max))
#> group_by: one grouping variable (Species)
#> summarize_all: now 3 rows and 9 columns, ungrouped
```

### tally, count, add\_tally, add\_count

``` r
a <- mtcars %>% group_by(gear, carb) %>% tally
#> group_by: 2 grouping variables (gear, carb)
#> tally: now 11 rows and 3 columns, one group variable remaining (gear)
b <- mtcars %>% group_by(gear, carb) %>% add_tally()
#> group_by: 2 grouping variables (gear, carb)
#> add_tally (grouped): new variable 'n' (integer) with 5 unique values and 0% NA

c <- mtcars %>% count(gear, carb)
#> count: now 11 rows and 3 columns, one group variable remaining (gear)
d <- mtcars %>% add_count(gear, carb, name = "count")
#> add_count: new variable 'count' (integer) with 5 unique values and 0% NA
```

### pivot\_longer, pivot\_wider

``` r
longer <- mtcars %>%
    mutate(id = 1:n()) %>%
    pivot_longer(-id, names_to = "var", values_to = "value")
#> mutate: new variable 'id' (integer) with 32 unique values and 0% NA
#> pivot_longer: reorganized (mpg, cyl, disp, hp, drat, …) into (var, value) [was 32x12, now 352x3]
wider <- longer %>%
    pivot_wider(names_from = var, values_from = value)
#> pivot_wider: reorganized (var, value) into (mpg, cyl, disp, hp, drat, …) [was 352x3, now 32x12]
```

Tidylog also supports `gather` and `spread`.

## Turning logging off, registering additional loggers

To turn off the output for just a particular function call, you can
simply call the dplyr and tidyr functions directly, e.g. `dplyr::filter`
or `tidyr::drop_na`.

To turn off the output more permanently, set the global option
`tidylog.display` to an empty list:

``` r
options("tidylog.display" = list())  # turn off
a <- filter(mtcars, mpg > 20)

options("tidylog.display" = NULL)    # turn on
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%), 14 rows remaining
```

This option can also be used to register additional loggers. The option
`tidylog.display` expects a list of functions. By default (when
`tidylog.display` is set to NULL), tidylog will use the `message`
function to display the output, but if you prefer a more colorful
output, simply overwrite the option:

``` r
library("crayon")  # for terminal colors
crayon <- function(x) cat(red$bold(x), sep = "\n")
options("tidylog.display" = list(crayon))
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%), 14 rows remaining
```

To print the output both to the screen and to a file, you could use:

``` r
log_to_file <- function(text) cat(text, file = "log.txt", sep = "\n", append = TRUE)
options("tidylog.display" = list(message, log_to_file))
a <- filter(mtcars, mpg > 20)
#> filter: removed 18 rows (56%), 14 rows remaining
```

## Namespace conflicts

Tidylog redefines several of the functions exported by dplyr and tidyr,
so it should be loaded last, otherwise there will be no output. A more
explicit way to resolve namespace conflicts is to use the
[conflicted](https://CRAN.R-project.org/package=conflicted) package:

``` r
library("dplyr")
library("tidyr")
library("tidylog")
library("conflicted")
for (f in getNamespaceExports("tidylog")) {
    conflicted::conflict_prefer(f, "tidylog", quiet = TRUE)
}
```
