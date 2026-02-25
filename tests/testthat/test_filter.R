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

test_that("sample_n, sample_frac", {
    expect_message({
        set.seed(1)
        out <- tidylog::sample_n(mtcars, 3)
    })
    set.seed(1)
    expect_equal(out, dplyr::sample_n(mtcars, 3))

    expect_message({
        set.seed(1)
        out <- tidylog::sample_frac(mtcars, .5)
    })
    set.seed(1)
    expect_equal(out, dplyr::sample_frac(mtcars, .5))
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

test_that("slice_*", {
    expect_message({
        out <- tidylog::slice(mtcars, 5:n())
    })
    expect_equal(out, dplyr::slice(mtcars, 5:n()))

    expect_message({
        out <- tidylog::slice_head(mtcars, n = 5)
    })
    expect_equal(out, dplyr::slice_head(mtcars, n = 5))

    expect_message({
        out <- tidylog::slice_tail(mtcars, n = 3)
    })
    expect_equal(out, dplyr::slice_tail(mtcars, n = 3))

    expect_message({
        out <- tidylog::slice_min(mtcars, mpg, n = 5)
    })
    expect_equal(out, dplyr::slice_min(mtcars, mpg, n = 5))

    expect_message({
        out <- tidylog::slice_max(mtcars, mpg, n = 3)
    })
    expect_equal(out, dplyr::slice_max(mtcars, mpg, n = 3))

    expect_message({
        set.seed(1)
        out <- tidylog::slice_sample(mtcars, n = 5)
    })
    set.seed(1)
    expect_equal(out, dplyr::slice_sample(mtcars, n = 5))

    expect_message({
        set.seed(1)
        out <- tidylog::slice_sample(mtcars, n = 5, replace = TRUE)
    })
    set.seed(1)
    expect_equal(out, dplyr::slice_sample(mtcars, n = 5, replace = TRUE)  )

    expect_message({
        set.seed(1)
        out <- tidylog::slice_sample(mtcars, weight_by = wt, n = 5)
    })
    set.seed(1)
    expect_equal(out, dplyr::slice_sample(mtcars, weight_by = wt, n = 5))
})

test_that("filter on grouped data", {
    gb <- group_by(mtcars, gear)
    expect_message({
        out <- tidylog::filter(gb, am == 0)
    }, "grouped")
    expect_message({
        out <- tidylog::filter(gb, am == 0)
    }, "removed one group, 2 groups remaining")
})


build_ties_msg <- function(func, data, ...) {
    # func should be one of dplyr::slice_min or dplyr::slice_max

    n_with_ties <- nrow(func(data, ..., with_ties = TRUE))
    n_without_ties <- nrow(func(data, ..., with_ties = FALSE))
    nties <- n_with_ties - n_without_ties

    glue::glue(".*with_ties\\: {nties} rows are ties")
}


test_that("slice_min with ties", {

    build_msg <- function(data, ...) {
        build_ties_msg(dplyr::slice_min, data, ...)
    }

    # Grouped explicitly
    gb <- dplyr::group_by(mtcars, gear)
    expect_message(tidylog::slice_min(gb, carb),
                   build_msg(gb, carb))
    expect_message(tidylog::slice_min(gb, carb, n=1),
                   build_msg(gb, carb, n=1))
    expect_message(tidylog::slice_min(gb, carb, prop=0.2),
                   build_msg(gb, carb, prop=0.2))
    expect_no_message(tidylog::slice_min(gb, carb, with_ties = FALSE),
                      message = build_msg(gb, carb))

    # Grouped using by=
    expect_message(tidylog::slice_min(mtcars, carb, by=gear),
                   build_msg(mtcars, carb, by=gear))
    expect_message(tidylog::slice_min(mtcars, carb, n=1, by=gear),
                   build_msg(mtcars, carb, n=1, by=gear))
    expect_message(tidylog::slice_min(mtcars, carb, prop=0.2, by=gear),
                   build_msg(mtcars, carb, prop=0.2, by=gear))
    expect_no_message(tidylog::slice_min(mtcars, carb, with_ties = FALSE, by=gear),
                      message = build_msg(mtcars, carb, by=gear))

    # Ungrouped
    expect_message(tidylog::slice_min(mtcars, carb),
                   build_msg(mtcars, carb))
    expect_message(tidylog::slice_min(mtcars, carb, n=1),
                   build_msg(mtcars, carb, n=1))
    expect_message(tidylog::slice_min(mtcars, carb, prop=0.2),
                   build_msg(mtcars, carb, prop=0.2))
    expect_no_message(tidylog::slice_min(mtcars, carb, with_ties = FALSE),
                      message = build_msg(mtcars, carb))
})

test_that("slice_max with ties", {

    build_msg <- function(data, ...) {
        build_ties_msg(dplyr::slice_max, data, ...)
    }

    # Grouped explicitly
    gb <- dplyr::group_by(mtcars, gear)
    expect_message(tidylog::slice_max(gb, carb),
                   build_msg(gb, carb))
    expect_message(tidylog::slice_max(gb, carb, n=1),
                   build_msg(gb, carb, n=1))
    expect_message(tidylog::slice_max(gb, carb, prop=0.2),
                   build_msg(gb, carb, prop=0.2))
    expect_no_message(tidylog::slice_max(gb, carb, with_ties = FALSE),
                      message = build_msg(gb, carb))

    # Grouped using by=
    expect_message(tidylog::slice_max(mtcars, carb, by=gear),
                   build_msg(mtcars, carb, by=gear))
    expect_message(tidylog::slice_max(mtcars, carb, n=1, by=gear),
                   build_msg(mtcars, carb, n=1, by=gear))
    expect_message(tidylog::slice_max(mtcars, carb, prop=0.2, by=gear),
                   build_msg(mtcars, carb, prop=0.2, by=gear))
    expect_no_message(tidylog::slice_max(mtcars, carb, with_ties = FALSE, by=gear),
                      message = build_msg(mtcars, carb, by=gear))

    # Ungrouped
    expect_message(tidylog::slice_max(mtcars, vs),
                   build_msg(mtcars, vs))
    expect_message(tidylog::slice_max(mtcars, vs, n=1),
                   build_msg(mtcars, vs, n=1))
    expect_message(tidylog::slice_max(mtcars, vs, prop=0.2),
                   build_msg(mtcars, vs, prop=0.2))
    expect_no_message(tidylog::slice_max(mtcars, vs, with_ties = FALSE),
                      message = build_msg(mtcars, vs))

})

test_that("slice_min/max with_ties=TRUE and multiple groups", {
    expect_message(
        tidylog::slice_min(mtcars, carb, by=c(am, gear), n=1),
        build_ties_msg(dplyr::slice_min, mtcars, carb, by=c(am, gear), n=1)
    )

    expect_message(
        tidylog::slice_max(mtcars, carb, by=c(am, gear), n=1),
        build_ties_msg(dplyr::slice_max, mtcars, carb, by=c(am, gear), n=1)
    )
})
