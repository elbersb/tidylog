library("dplyr")
library("tidylog")
context("test_select")

test_that("select", {
    expect_message({
        out <- select(mtcars, mpg, cyl)
    })
    expect_equal(ncol(out), ncol(dplyr::select(mtcars, mpg, cyl)))

    expect_message({
        out <- select(mtcars, -(mpg:carb))
    })
    expect_equal(ncol(out), 0)

    expect_silent({
        out <- dplyr::select(mtcars, mpg, cyl)
    })
})

test_that("select: scoped variants", {

})

test_that("select: argument order", {
    expect_message({
        out <- select(mpg, cyl, .data = mtcars)
    })
    expect_equal(ncol(out), ncol(dplyr::select(mtcars, mpg, cyl)))
})
