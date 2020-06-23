context("test_mutate")
library("dplyr")
library("tidylog")

test_that("mutate", {
    expect_message({
        out <- tidylog::mutate(mtcars, test = TRUE)
    })
    expect_equal(all(out$test), TRUE)

    # factor to factor
    f <- function() tidylog::mutate(iris, Species = recode(Species, "virginica" = "v"))
    expect_message(f(), "changed 50 values.*0 new NA")

    # factor to factor, with missings
    f <- function() tidylog::mutate(iris, Species = na_if(Species, "virginica"))
    expect_message(f(), "changed 50 values.*50 new NA")

    # numeric to numeric
    f <- function() tidylog::mutate(iris, Sepal.Length = round(Sepal.Length))
    expect_message(f(), "changed 133 values.*0 new NA")

    # character to character
    iris2 <- dplyr::mutate(iris, Species = as.character(Species))
    f <- function() tidylog::mutate(iris2, Species = ifelse(Species == "virginica", "v", Species))
    expect_message(f(), "changed 50 values.*0 new NA")

    # factor to character
    f <- function() tidylog::mutate(iris, Species = as.character(Species))
    expect_message(f(), "from factor to character.*0 new NA")

    # factor to 100% missing
    f <- function() tidylog::mutate(iris, Species = NA)
    expect_message(f(), "now 100% NA")

    # double to character
    f <- function() tidylog::mutate(iris, Sepal.Length = as.character(Sepal.Length))
    expect_message(f(), "from double to character.*0 new NA")

    # character to ordered factor
    iris2 <- dplyr::mutate(iris, Species = as.character(Species))
    f <- function() tidylog::mutate(iris2, Species = ordered(Species))
    expect_message(f(), "from character to ordered factor.*0 new NA")

    # ordered factor to character
    iris2 <- dplyr::mutate(iris, Species = ordered(Species))
    f <- function() tidylog::mutate(iris2, Species = as.character(Species))
    expect_message(f(), "from ordered factor to character.*0 new NA")

    # character to Date
    iris2 <- dplyr::mutate(iris, date = "2016-01-01")
    f <- function() tidylog::mutate(iris2, date = as.Date(date))
    expect_message(f(), "from character to Date.*0 new NA")
})

test_that("missings", {
    # same type, increase missings
    f <- function() tidylog::mutate(iris, Sepal.Length = ifelse(Sepal.Length > 5, NA, Sepal.Length))
    expect_message(f(), "changed.*(118 new NA)")

    # same type, reduce missings
    iris2 <- dplyr::mutate(iris, Sepal.Length = ifelse(Sepal.Length > 5, NA, Sepal.Length))
    f <- function() tidylog::mutate(iris2, Sepal.Length = 1)
    expect_message(f(), "changed.*(118 fewer NA)")

    # same type, same missings
    f <- function() tidylog::mutate(iris, Sepal.Length = sample(Sepal.Length))
    expect_message(f(), "changed.*(0 new NA)")

    # double to integer, increase missings
    f <- function() tidylog::mutate(iris,
        Sepal.Length = as.integer(ifelse(Sepal.Length > 5, NA, Sepal.Length)))
    expect_message(f(), "converted.*to integer.*(118 new NA)")

    # double to integer, reduce missings
    iris2 <- dplyr::mutate(iris, Sepal.Length = ifelse(Sepal.Length > 5, NA, Sepal.Length))
    f <- function() tidylog::mutate(iris2, Sepal.Length = as.integer(1))
    expect_message(f(), "converted.*to integer.*(118 fewer NA)")

    # double to integer, same missings
    f <- function() tidylog::mutate(iris,
        Sepal.Length = as.integer(sample(Sepal.Length)))
    expect_message(f(), "converted.*to integer.*(0 new NA)")
})

test_that("transmute", {
    expect_message({
        out <- tidylog::transmute(mtcars, test = TRUE)
    })
    expect_equal(all(out$test), TRUE)
    expect_equal(ncol(out), 1)

    f <- function() tidylog::transmute(mtcars)
    expect_message(f(), regexp = "dropped all variables")
})

test_that("percent function", {
    iris2 <- bind_rows(iris, iris)

    # 100% change
    f <- function() tidylog::mutate(iris2, Sepal.Length = 1)
    expect_message(f(), regexp = "100%")

    # <1% change, but no 0%
    f <- function() tidylog::mutate(iris2,
            Sepal.Length = ifelse(row_number() == 1, 100, Sepal.Length))
    expect_message(f(), regexp = "<1%")

    # >99% change, but no 100%
    f <- function() tidylog::mutate(iris2,
            Sepal.Length = ifelse(row_number() != 1, 100, Sepal.Length))
    expect_message(f(), regexp = ">99%")

    # no changes
    f <- function() tidylog::mutate(iris2, Sepal.Length = Sepal.Length)
    expect_message(f(), regexp = "no changes")

    # 0%
    f <- function() tidylog::mutate(iris2, test = 1)
    expect_message(f(), regexp = "0%")
})

test_that("add_tally", {
    expect_message({
        out <- mtcars %>%
            tidylog::group_by(cyl) %>%
            tidylog::add_tally()
    })
    expect_equal(nrow(out), nrow(mtcars))
    expect_equal(ncol(out), ncol(mtcars) + 1)
})

test_that("add_count", {
    expect_message({
        out <- mtcars %>%
            tidylog::group_by(gear) %>%
            tidylog::add_count(carb)
    })
    expect_equal(nrow(out), nrow(mtcars))
    expect_equal(ncol(out), ncol(mtcars) + 1)
})

test_that("mutate: scoped variants", {
    expect_message({
        out <- tidylog::mutate_all(mtcars, round)
    })
    expect_equal(out, dplyr::mutate_all(mtcars, round))

    expect_message({
        out <- tidylog::mutate_at(mtcars, vars(mpg:disp), scale)
    })
    expect_equal(out, dplyr::mutate_at(mtcars, vars(mpg:disp), scale))

    expect_message({
        out <- tidylog::mutate_if(iris, is.factor, as.character)
    })
    expect_equal(out, dplyr::mutate_if(iris, is.factor, as.character))
})

test_that("transmute: scoped variants", {
    expect_message({
        out <- tidylog::transmute_all(mtcars, round)
    })
    expect_equal(out, dplyr::transmute_all(mtcars, round))

    expect_message({
        out <- tidylog::transmute_at(mtcars, vars(mpg:disp), scale)
    })
    expect_equal(out, dplyr::transmute_at(mtcars, vars(mpg:disp), scale))

    expect_message({
        out <- tidylog::transmute_if(iris, is.factor, as.character)
    })
    expect_equal(out, dplyr::transmute_if(iris, is.factor, as.character))
})

test_that("mutate: argument order", {
    expect_message({
        out <- tidylog::mutate(test = TRUE, .data = mtcars)
    })
    expect_equal(all(out$test), TRUE)
})

test_that("transmute: argument order", {
    expect_message({
        out <- tidylog::transmute(test = TRUE, .data = mtcars)
    })
    expect_equal(all(out$test), TRUE)
    expect_equal(ncol(out), 1)
})

test_that("mutate/transmute: partial matching", {
    f <- function() tidylog::mutate(mtcars, f = 1)
    expect_message(f(), "new variable 'f'")
    f <- function() tidylog::transmute(mtcars, fun = 1)
    expect_message(f(), "new variable 'fun'")
})

test_that("tidyr::replace_na", {
    df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))

    expect_message({
        out <- tidylog::replace_na(df, list(x = 0, y = "unknown"))
    })
    # with vector
    expect_silent(dplyr::mutate(df, x = replace_na(x, 0)))

    expect_equal(tidyr::replace_na(df, list(x = 0, y = "unknown")), out)
})

test_that("tidyr::fill", {
    df <- data.frame(Month = 1:12, Year = c(2000, rep(NA, 11)))
    expect_message({
        out <- tidylog::fill(df, Year)
    })

    expect_equal(tidyr::fill(df, Year), out)
})

test_that("mutate: list columns", {
    df <- tibble(x = list(1, 2))
    # from list to scalar
    expect_message({
        out <- tidylog::mutate(df, x = 1)
    }, "converted.*from list to")
    # from list to identical list
    expect_message({
        out <- tidylog::mutate(df, x = list(1, 2))
    }, "no changes")
    # from list to different list
    expect_message({
        out <- tidylog::mutate(df, x = lapply(x, function(v) v * 100))
    }, "changed 2 values")
    # from scalar to list
    df <- tibble(x = 1:2)
    expect_message({
        out <- tidylog::mutate(df, x = as.list(x))
    }, "converted.*from integer to list")
})

test_that("mutate: ordered factors", {
    df <- tibble(x = ordered(c("apple", "bear", "banana", "dear")))
    rec <- forcats::fct_recode(df$x, fruit = "apple", fruit = "banana")
    # new variable
    expect_message({
        new <- tidylog::mutate(df,
                    y = forcats::fct_recode(x, fruit = "apple", fruit = "banana"))
    }, "new variable 'y' (ordered factor) with 3 unique values", fixed = TRUE)
    expect_true(all(new$y == rec))

    # overwriting
    expect_message({
        new <- tidylog::mutate(df,
                    x = forcats::fct_recode(x, fruit = "apple", fruit = "banana"))
    }, "changed 2 values")
    expect_true(all(new$x == rec))
})

test_that("mutate: variable type", {
    expect_message({
        tidylog::mutate(tibble(x = 1:10), y = 1)
    }, "new variable 'y' (double)", fixed = TRUE)

    expect_message({
        tidylog::mutate(tibble(x = 1:10), y = "a")
    }, "new variable 'y' (character)", fixed = TRUE)

    expect_message({
        tidylog::mutate(tibble(x = 1:10), y = as.factor("a"))
    }, "new variable 'y' (factor)", fixed = TRUE)
})
