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

test_that("slice_min with ties", {

    build_msg <- function(nties, ngroups) {
        group_suffix <- if(ngroups == 1) "" else glue::glue(" \\(across {ngroups} groups\\)")
        glue::glue(".*slice_min: {nties} rows are ties{group_suffix}")
    }

    grouped_n_msg <- build_msg(6, 3)  # n=1
    grouped_prop_msg <- build_msg(3, 2)  # prop=0.2

    # Grouped explicitly
    gb <- dplyr::group_by(mtcars, gear)
    expect_message(tidylog::slice_min(gb, carb), grouped_n_msg)
    expect_message(tidylog::slice_min(gb, carb, n=1), grouped_n_msg)
    expect_message(tidylog::slice_min(gb, carb, prop=0.2), grouped_prop_msg)
    expect_no_message(tidylog::slice_min(gb, carb, with_ties = FALSE),
                      message = grouped_n_msg)

    # Grouped using by=
    expect_message(tidylog::slice_min(mtcars, carb, by=gear), grouped_n_msg)
    expect_message(tidylog::slice_min(mtcars, carb, by=gear, n=1), grouped_n_msg)
    expect_message(tidylog::slice_min(mtcars, carb, by=gear, prop=0.2), grouped_prop_msg)
    expect_no_message(tidylog::slice_min(mtcars, carb, by=gear, with_ties = FALSE),
                      message = grouped_n_msg)

    # Ungrouped
    ungrouped_msg <- ".*slice_min: 6 rows are ties"
    expect_message(tidylog::slice_min(mtcars, carb), ungrouped_msg)
    expect_message(tidylog::slice_min(mtcars, carb, n=1), ungrouped_msg)
    expect_message(tidylog::slice_min(mtcars, carb, prop=0.04), ungrouped_msg)
    expect_no_message(tidylog::slice_min(mtcars, carb, with_ties = FALSE),
                      message = ungrouped_msg)

})

test_that("slice_max with ties", {

    build_msg <- function(nties, ngroups) {
        group_suffix <- if(ngroups == 1) "" else glue::glue(" \\(across {ngroups} groups\\)")
        glue::glue(".*slice_max: {nties} rows are ties{group_suffix}")
    }

    grouped_n_msg <- build_msg(7, 2)
    grouped_prop_msg <- build_msg(4, 2)

    # Grouped explicitly
    gb <- dplyr::group_by(mtcars, gear)
    expect_message(tidylog::slice_max(gb, carb), grouped_n_msg)
    expect_message(tidylog::slice_max(gb, carb, n=1), grouped_n_msg)
    expect_message(tidylog::slice_max(gb, carb, prop=0.2), grouped_prop_msg)
    expect_no_message(tidylog::slice_max(gb, carb, with_ties = FALSE),
                      message = grouped_n_msg)

    # Grouped using by=
    expect_message(tidylog::slice_max(mtcars, carb, by=gear), grouped_n_msg)
    expect_message(tidylog::slice_max(mtcars, carb, by=gear, n=1), grouped_n_msg)
    expect_message(tidylog::slice_max(mtcars, carb, by=gear, prop=0.2), grouped_prop_msg)
    expect_no_message(tidylog::slice_max(mtcars, carb, by=gear, with_ties = FALSE),
                      message = grouped_n_msg)

    # Ungrouped
    ungrouped_msg <- ".*slice_max: 13 rows are ties"
    expect_message(tidylog::slice_max(mtcars, vs), ungrouped_msg)
    expect_message(tidylog::slice_max(mtcars, vs, n=1), ungrouped_msg)
    expect_message(tidylog::slice_max(mtcars, vs, prop=0.04), ungrouped_msg)
    expect_no_message(tidylog::slice_max(mtcars, vs, with_ties = FALSE),
                      message = ungrouped_msg)

})

test_that("slice_min/max with_ties=TRUE and multiple groups", {
    expect_message(
        tidylog::slice_min(mtcars, carb, by=c(am, gear), n=1),
        ".*slice_min: 7 rows are ties \\(across 4 groups\\)"
    )

    expect_message(
        tidylog::slice_max(mtcars, carb, by=c(am, gear), n=1),
        ".*slice_max: 6 rows are ties \\(across 3 groups\\)"
    )
})
