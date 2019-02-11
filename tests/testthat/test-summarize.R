library("dplyr")
library("tidylog")
context("test-summarize")

test_that("summarize", {
    expect_message({
        out <- summarize(mtcars, test = TRUE)
    })
    expect_equal(nrow(out), 1)

    expect_message({
        out <- mtcars %>%
            group_by(cyl, carb) %>%
            summarize(avg_mpg = mean(mpg))
    })
    expect_equal(nrow(out), 9)

    expect_silent({
        out <- dplyr::summarise(mtcars)
    })
})

test_that("summarize: scoped variants", {
    expect_message({
        out <- summarise_all(mtcars, max)
    })
    expect_equal(nrow(out), 1)

    expect_message({
        out <- summarise_at(mtcars, c("mpg", "hp"), mean)
    })
})

test_that("summarize: argument order", {
    expect_message({
        out <- summarize(avg = mean(mpg), .data = mtcars)
    })
    expect_equal(nrow(out), 1)
})
