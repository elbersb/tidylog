context("test_select")
library("dplyr")
library("tidylog")

test_that("select", {
    expect_message({
        out <- tidylog::select(mtcars, mpg, cyl)
    })
    expect_equal(ncol(out), ncol(dplyr::select(mtcars, mpg, cyl)))

    expect_message({
        out <- tidylog::select(mtcars, - (mpg:carb))
    })
    expect_equal(ncol(out), 0)

    expect_silent({
        out <- dplyr::select(mtcars, mpg, cyl)
    })
})

test_that("select: scoped variants", {
    is_whole <- function(x) all(floor(x) == x)

    expect_message({
        out <- tidylog::select_all(mtcars, toupper)
    })
    expect_equal(out, dplyr::select_all(mtcars, toupper))

    expect_message({
        out <- tidylog::select_if(mtcars, is_whole, toupper)
    })
    expect_equal(out, dplyr::select_if(mtcars, is_whole, toupper))

    expect_message({
        out <- tidylog::select_at(mtcars, vars(-everything()))
    })
    expect_equal(out, dplyr::select_at(mtcars, vars(-everything())))

})

test_that("select: argument order", {
    expect_message({
        out <- tidylog::select(mpg, cyl, .data = mtcars)
    })
    expect_equal(ncol(out), ncol(dplyr::select(mtcars, mpg, cyl)))
})
