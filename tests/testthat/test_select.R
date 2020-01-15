context("test_select")
library("dplyr")
library("tidylog")

test_that("select", {
    # no changes
    f <- function() tidylog::select(mtcars, everything())
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::select(mtcars, everything()))

    # only order changed
    f <- function() tidylog::select(mtcars, hp, everything())
    expect_message(out <- f(), "columns reordered")
    expect_equal(out, dplyr::select(mtcars, hp, everything()))

    # dropped some
    f <- function() tidylog::select(mtcars, -mpg, -cyl, -disp)
    expect_message(out <- f(), "dropped 3")
    expect_equal(out, dplyr::select(mtcars, -mpg, -cyl, -disp))

    # dropped all
    f <- function() tidylog::select(mtcars, -everything())
    expect_message(out <- f(), "dropped all variables")
    expect_equal(out, dplyr::select(mtcars, -everything()))

    # renamed
    small_mtcars <- dplyr::select(mtcars, mpg, cyl, hp)
    f <- function() tidylog::select(small_mtcars, a = mpg, b = cyl, c = hp)
    expect_message(out <- f(), "renamed 3")
    expect_equal(out, dplyr::select(small_mtcars, a = mpg, b = cyl, c = hp))

    # dropped and renamed
    f <- function() tidylog::select(small_mtcars, a = mpg, b = cyl)
    expect_message(out <- f(), "renamed 2.*dropped one")
    expect_equal(out, dplyr::select(small_mtcars, a = mpg, b = cyl))

    # dplyr call is silent
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
