context("test_rename")
library("dplyr")
library("tidylog")

test_that("rename", {
    expect_message({
        out <- tidylog::rename(mtcars, MPG = mpg, CYL = cyl)
    })
    expect_equal(ncol(out), ncol(dplyr::rename(mtcars, MPG = mpg, CYL = cyl)))

    expect_silent({
        out <- dplyr::rename(mtcars, MPG = mpg, CYL = cyl)
    })
})

test_that("rename: scoped variants", {
    is_whole <- function(x) all(floor(x) == x)

    expect_message({
        out <- tidylog::rename_all(mtcars, toupper)
    })
    expect_equal(out, dplyr::rename_all(mtcars, toupper))

    expect_message({
        out <- tidylog::rename_if(mtcars, is_whole, toupper)
    })
    expect_equal(out, dplyr::rename_if(mtcars, is_whole, toupper))

    expect_message({
        out <- tidylog::rename_at(mtcars, vars(-(1:3)), toupper)
    })
    expect_equal(out, dplyr::rename_at(mtcars, vars(-(1:3)), toupper))

})

test_that("rename: argument order", {
    expect_message({
        out <- tidylog::rename(MPG = mpg, CYL = cyl, .data = mtcars)
    })
    expect_equal(ncol(out), ncol(dplyr::rename(mtcars, MPG = mpg, CYL = cyl)))
})
