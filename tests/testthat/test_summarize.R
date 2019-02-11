library("dplyr")
library("tidylog")
context("test_summarize")

test_that("summarize", {
    expect_message({
        out <- tidylog::summarize(mtcars, test = TRUE)
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

test_that("summarize: scoped variants", {
    expect_message({
        out <- tidylog::summarise_all(mtcars, max)
    })
    expect_equal(nrow(out), 1)

    expect_message({
        out <- tidylog::summarise_at(mtcars, c("mpg", "hp"), mean)
    })
})

test_that("summarize: argument order", {
    expect_message({
        out <- tidylog::summarize(avg = mean(mpg), .data = mtcars)
    })
    expect_equal(nrow(out), 1)
})
