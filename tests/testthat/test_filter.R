context("test_filter")
library("dplyr")
library("tidyr")
library("tidylog")

test_that("filter", {
    expect_message({
        out <- tidylog::filter(mtcars, mpg > 1000)
    })
    expect_equal(nrow(out), 0)

    expect_message({
        out <- tidylog::filter(mtcars, mpg < 1000)
    })
    expect_equal(nrow(out), nrow(mtcars))

    expect_message({
        out <- tidylog::filter(mtcars, mpg == 21)
    })
    expect_equal(nrow(out), 2)

    expect_silent({
        out <- dplyr::filter(mtcars, mpg == 21)
    })
})

test_that("distinct", {
    expect_message({
        out <- tidylog::distinct(mtcars, mpg)
    })
    expect_equal(out, dplyr::distinct(mtcars, mpg))
})

test_that("top_n, top_frac", {
    expect_message({
        out <- tidylog::top_n(mtcars, 3, carb)
    })
    expect_equal(out, dplyr::top_n(mtcars, 3, carb))

    expect_message({
        out <- tidylog::top_frac(mtcars, .5, carb)
    })
    expect_equal(out, dplyr::top_frac(mtcars, .5, carb))
})

test_that("filter: scoped variants", {
    expect_message({
        out <- tidylog::filter_all(mtcars, all_vars(. > 150))
    })
    expect_equal(out, dplyr::filter_all(mtcars, all_vars(. > 150)))

    expect_message({
        out <- tidylog::filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))
    })
    expect_equal(out, dplyr::filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0)))

    expect_message({
        out <- tidylog::filter_at(mtcars, vars(starts_with("d")), any_vars(. %% 2 == 0))
    })
    expect_equal(out, dplyr::filter_at(mtcars, vars(starts_with("d")), any_vars(. %% 2 == 0)))
})

test_that("distinct: scoped variants", {
    df <- tibble(x = rep(2:5, each = 2) / 2, y = rep(2:3, each = 4) / 2)

    expect_message({
        out <- tidylog::distinct_all(df)
    })
    expect_equal(out, dplyr::distinct_all(df))

    expect_message({
        out <- tidylog::distinct_if(df, is.numeric)
    })
    expect_equal(out, dplyr::distinct_if(df, is.numeric))

    expect_message({
        out <- tidylog::distinct_at(df, vars(x, y))
    })
    expect_equal(out, dplyr::distinct_at(df, vars(x, y)))
})

test_that("filter: argument order", {
    expect_message({
        out <- tidylog::filter(mpg == 21, .data = mtcars)
    })
    expect_equal(nrow(out), 2)

    expect_message({
        out <- tidylog::distinct(mpg, .data = mtcars)
    })
    expect_equal(nrow(out), nrow(dplyr::distinct(mtcars, mpg)))
})

test_that("drop_na", {
    expect_message({
        out <- tidylog::drop_na(airquality, Ozone)
    })
    expect_equal(nrow(out), nrow(tidyr::drop_na(airquality, Ozone)))

    expect_message({
        out <- tidylog::drop_na(airquality, Wind, Temp, Month, Day)
    })
    expect_equal(nrow(out), nrow(airquality))

    expect_message({
        out <- tidylog::drop_na(airquality)
    })
    expect_equal(nrow(out), nrow(na.omit(airquality)))

    expect_message({
        out <- tidylog::drop_na(airquality, Solar.R)
    })
    expect_equal(nrow(out), 146)

})
