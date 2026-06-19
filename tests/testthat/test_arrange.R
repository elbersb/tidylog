context("test_arrange")
library("dplyr")
library("tidyr")
library("tidylog")

.ellipsis <- cli::symbol$ellipsis

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
    expect_message(out <- f(), "sorted rows by carb")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # ungrouped: many columns with desc()
    f <- function() tidylog::arrange(mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(), "sorted rows by carb, desc\\(gear\\), mpg")
    expect_equal(out, dplyr::arrange(mtcars, carb, desc(gear), mpg))

    # ungrouped: >5 columns only shows the first 5
    f <- function() tidylog::arrange(mtcars, mpg, cyl, disp, hp, drat, wt)
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, mpg, cyl, disp, hp, drat, wt))

    # grouped:
    grp_mtcars <- dplyr::group_by(mtcars, cyl)
    f <- function() tidylog::arrange(grp_mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(),
                   "\\(grouped\\): sorted rows within groups by carb, desc\\(gear\\), mpg")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, desc(gear), mpg))
})

test_that("arrange parses across correctly", {
    # across(everything())
    f <- function() tidylog::arrange(mtcars, across(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, across(everything())))


    # across(starts_with())
    f <- function() tidylog::arrange(mtcars, across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"))))

    # Empty across() is treated the same as everything()
    # and transmits the dplyr warning through.
    f <- function() tidylog::arrange(mtcars, across())
    expect_warning(
        expect_message(out <- f(),
                       glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    )
    suppressWarnings(
        expect_equal(out, dplyr::arrange(mtcars, across()))
    )


    # across(c())
    f <- function() tidylog::arrange(mtcars, across(c()))
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, across(c())))


})

test_that("arrange parses pick correctly", {
    # pick(everything())
    f <- function() tidylog::arrange(mtcars, pick(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, pick(everything())))


    # pick(starts_with())
    f <- function() tidylog::arrange(mtcars, pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, pick(starts_with("c"))))

    # Empty pick() errors like dplyr::pick()
    expect_equal(
        tryCatch(dplyr::arrange(mtcars, pick()), error = conditionMessage),
        tryCatch(tidylog::arrange(mtcars, pick()), error = conditionMessage)
    )

    # pick(c())
    f <- function() tidylog::arrange(mtcars, pick(c()))
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, pick(c())))


})
