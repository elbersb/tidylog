library("dplyr")
library("tidylog")
context("test_filter")

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
    expect_equal(nrow(out), nrow(dplyr::distinct(mtcars, mpg)))
})

test_that("filter: scoped variants", {

})

test_that("distinct: scoped variants", {

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
