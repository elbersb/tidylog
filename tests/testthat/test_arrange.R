context("test_arrange")
library("dplyr")
library("tidyr")
library("tidylog")

test_that("arrange", {

    # empty has no effect and does not error
    f <- function() tidylog::arrange(mtcars)
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars))

    # no changes because already sorted
    input <- dplyr::arrange(mtcars, carb)
    f <- function() tidylog::arrange(input, carb)
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # ungrouped: one column
    f <- function() tidylog::arrange(mtcars, carb)
    expect_message(out <- f(), "arranged rows by carb")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # ungrouped: many columns with desc()
    f <- function() tidylog::arrange(mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(), "arranged rows by carb, desc\\(gear\\), mpg")
    expect_equal(out, dplyr::arrange(mtcars, carb, desc(gear), mpg))


    # grouped:
    grp_mtcars <- dplyr::group_by(mtcars, cyl)
    f <- function() tidylog::arrange(grp_mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(),
                   "\\(grouped\\): arranged rows by carb, desc\\(gear\\), mpg \\(within groups\\)")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, desc(gear), mpg))
})
