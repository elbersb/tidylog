context("test_group_by")
library("dplyr")
library("tidylog")

test_that("group_by", {
    expect_message({
        out <- tidylog::group_by(mtcars, mpg)
    })
    expect_equal(is.grouped_df(out), TRUE)

    expect_silent({
        out <- dplyr::group_by(mtcars, mpg)
    })
})

test_that("group_by: scoped variants", {
    expect_message({
        out <- tidylog::group_by_all(mtcars)
    }, "11 grouping variables") # nolint

    expect_message({
        out <- tidylog::group_by_if(mtcars, is.numeric)
    }, "11 grouping variables") # nolint

    expect_message({
        out <- tidylog::group_by_at(mtcars, vars(vs:am))
    }, "2 grouping variables") # nolint
})

test_that("group_by: argument order", {
    expect_message({
        out <- tidylog::group_by(mpg, .data = mtcars)
    })
    expect_equal(is.grouped_df(out), TRUE)
})

test_that("ungroup", {
    expect_message({
        out <- tidylog::ungroup(mtcars)
    })
    expect_equal(dplyr::is.grouped_df(out), FALSE)

    expect_silent({
        out <- dplyr::ungroup(mtcars)
    })
    expect_silent({
        options("tidylog.display" = list())  # turn off
        out <- tidylog::ungroup(mtcars)
    })
    options("tidylog.display" = NULL)  # turn on
})
