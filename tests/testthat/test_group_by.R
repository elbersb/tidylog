library("dplyr")
library("tidylog")
context("test_group_by")

test_that("group_by", {
    expect_message({
        out <- group_by(mtcars, mpg)
    })
    expect_equal(is.grouped_df(out), TRUE)

    expect_silent({
        out <- dplyr::group_by(mtcars, mpg)
    })
})

test_that("group_by: scoped variants", {

})

test_that("group_by: argument order", {
    expect_message({
        out <- group_by(mpg, .data = mtcars)
    })
    expect_equal(is.grouped_df(out), TRUE)
})
