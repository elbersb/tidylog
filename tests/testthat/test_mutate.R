library("dplyr")
library("tidylog")
context("test_mutate")

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
