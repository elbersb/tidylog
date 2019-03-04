library("dplyr")
library("tidylog")
context("test_summarize")

test_that("summarize", {
    expect_message({
        out <- tidylog::summarize(mtcars, test = TRUE)
    })
    expect_equal(nrow(out), 1)

    expect_message({
        out <- tidylog::summarise(mtcars, test = TRUE)
    })
    expect_equal(nrow(out), 1)

    expect_message({
        out <- mtcars %>%
            tidylog::group_by(cyl, carb) %>%
            tidylog::summarize(avg_mpg = mean(mpg))
    })
    expect_equal(nrow(out), 9)

    expect_silent({
        out <- dplyr::summarise(mtcars)
    })
})

test_that("tally", {
    expect_message({
        out <- mtcars %>%
            tidylog::group_by(cyl) %>%
            tidylog::tally()
    })
    expect_equal(nrow(out), 3)
    expect_equal(ncol(out), 2)
})

test_that("count", {
    expect_message({
        out <- mtcars %>%
            tidylog::group_by(gear) %>%
            tidylog::count(carb)
    })
    expect_equal(nrow(out), 11)
    expect_equal(ncol(out), 3)
})

test_that("summarize: scoped variants", {
    expect_message({
        out <- tidylog::summarize_all(mtcars, max)
    })
    expect_equal(out, dplyr::summarize_all(mtcars, max))

    expect_message({
        out <- tidylog::summarise_all(mtcars, max)
    })
    expect_equal(out, dplyr::summarise_all(mtcars, max))

    expect_message({
        out <- tidylog::summarize_if(mtcars, is.numeric, mean, na.rm = TRUE)
    })
    expect_equal(out, dplyr::summarize_if(mtcars, is.numeric, mean, na.rm = TRUE))

    expect_message({
        out <- tidylog::summarise_if(mtcars, is.numeric, mean, na.rm = TRUE)
    })
    expect_equal(out, dplyr::summarise_if(mtcars, is.numeric, mean, na.rm = TRUE))

    expect_message({
        out <- tidylog::summarize_at(mtcars, c("mpg", "hp"), mean)
    })
    expect_equal(out, dplyr::summarize_at(mtcars, c("mpg", "hp"), mean))

    expect_message({
        out <- tidylog::summarise_at(mtcars, c("mpg", "hp"), mean)
    })
    expect_equal(out, dplyr::summarise_at(mtcars, c("mpg", "hp"), mean))
})

test_that("summarize: argument order", {
    expect_message({
        out <- tidylog::summarize(avg = mean(mpg), .data = mtcars)
    })
    expect_equal(nrow(out), 1)
})
