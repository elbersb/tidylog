library("dplyr")
library("tidylog")
context("test_filter")

test_that("filter", {
    expect_message({
        out <- filter(mtcars, mpg > 1000)
    })
    expect_equal(nrow(out), 0)

    expect_message({
        out <- filter(mtcars, mpg < 1000)
    })
    expect_equal(nrow(out), nrow(mtcars))

    expect_message({
        out <- filter(mtcars, mpg == 21)
    })
    expect_equal(nrow(out), 2)

    expect_silent({
        out <- dplyr::filter(mtcars, mpg == 21)
    })
})

test_that("filter: scoped variants", {

})

test_that("filter: argument order", {
    expect_message({
        out <- filter(mpg == 21, .data = mtcars)
    })
    expect_equal(nrow(out), 2)
})
