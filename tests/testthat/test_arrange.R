context("test_arrange")
library("dplyr")
library("tidyr")
library("tidylog")

.ellipsis <- cli::symbol$ellipsis

test_that("arrange: basic", {

    # empty has no effect and does not error
    f <- function() tidylog::arrange(mtcars)
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars))

    # no changes because already sorted
    input <- dplyr::arrange(mtcars, carb)
    f <- function() tidylog::arrange(input, carb)
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # single column
    f <- function() tidylog::arrange(mtcars, carb)
    expect_message(out <- f(), "sorted rows by carb")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # multiple columns
    f <- function() tidylog::arrange(mtcars, carb, gear, mpg)
    expect_message(out <- f(), "sorted rows by carb, gear, mpg")
    expect_equal(out, dplyr::arrange(mtcars, carb, gear, mpg))

    # >5 columns only shows the first 5
    f <- function() tidylog::arrange(mtcars, mpg, cyl, disp, hp, drat, wt)
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, mpg, cyl, disp, hp, drat, wt))
})

test_that("arrange: desc", {

    # single desc()
    f <- function() tidylog::arrange(mtcars, desc(carb))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\)")
    expect_equal(out, dplyr::arrange(mtcars, desc(carb)))

    # all desc()
    f <- function() tidylog::arrange(mtcars, desc(carb), desc(gear))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\), desc\\(gear\\)")
    expect_equal(out, dplyr::arrange(mtcars, desc(carb), desc(gear)))

    # mixed bare and desc()
    f <- function() tidylog::arrange(mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(), "sorted rows by carb, desc\\(gear\\), mpg")
    expect_equal(out, dplyr::arrange(mtcars, carb, desc(gear), mpg))

    # namespaced dplyr::desc()
    f <- function() tidylog::arrange(mtcars, dplyr::desc(carb))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\)")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::desc(carb)))
})

test_that("arrange: grouped", {
    grp_mtcars <- dplyr::group_by(mtcars, cyl)

    # grouped sorting
    f <- function() tidylog::arrange(grp_mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(),
                   "\\(grouped\\): sorted rows within groups by carb, desc\\(gear\\), mpg")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, desc(gear), mpg))
})

test_that("arrange excludes named arguments", {
    grp_mtcars <- dplyr::group_by(mtcars, cyl)

    # .by_group is excluded from display
    f <- function() tidylog::arrange(grp_mtcars, carb, .by_group = TRUE)
    expect_message(out <- f(), "sorted rows within groups by carb")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, .by_group = TRUE))

    # .locale is excluded from display
    f <- function() tidylog::arrange(mtcars, carb, .locale = "en")
    expect_message(out <- f(), "sorted rows by carb")
    expect_equal(out, dplyr::arrange(mtcars, carb, .locale = "en"))
})

test_that("arrange: programmatic", {

    # !!sym() is evaluated to the column name
    col <- "carb"
    f <- function() tidylog::arrange(mtcars, !!sym(col))
    expect_message(out <- f(), "sorted rows by carb")
    expect_equal(out, dplyr::arrange(mtcars, !!sym(col)))
})

test_that("arrange: across", {

    # across(everything())
    f <- function() tidylog::arrange(mtcars, across(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, across(everything())))

    # across(starts_with())
    f <- function() tidylog::arrange(mtcars, across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"))))

    # across(all_of())
    cols <- c("carb", "gear")
    f <- function() tidylog::arrange(mtcars, across(all_of(cols)))
    expect_message(out <- f(), "sorted rows by carb, gear")
    expect_equal(out, dplyr::arrange(mtcars, across(all_of(cols))))

    # across(c(a, b))
    f <- function() tidylog::arrange(mtcars, across(c(carb, gear)))
    expect_message(out <- f(), "sorted rows by carb, gear")
    expect_equal(out, dplyr::arrange(mtcars, across(c(carb, gear))))

    # across(a:b) unpacks intermediate columns
    f <- function() tidylog::arrange(mtcars, across(cyl:hp))
    expect_message(out <- f(), "sorted rows by cyl, disp, hp")
    expect_equal(out, dplyr::arrange(mtcars, across(cyl:hp)))

    # namespaced across()
    f <- function() tidylog::arrange(mtcars, dplyr::across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::across(starts_with("c"))))

    # across() matching no columns — no changes
    f <- function() tidylog::arrange(mtcars, across(starts_with("zzz")))
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("zzz"))))

    # empty across() treated as everything() and transmits dplyr deprecation warning
    f <- function() tidylog::arrange(mtcars, across())
    expect_warning(
        expect_message(out <- f(),
                       glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    )
    suppressWarnings(
        expect_equal(out, dplyr::arrange(mtcars, across()))
    )

    # across(c()) — selects nothing, no changes
    f <- function() tidylog::arrange(mtcars, across(c()))
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, across(c())))
})

test_that("arrange: across with desc", {

    # desc as function arg
    f <- function() tidylog::arrange(mtcars, across(starts_with("c"), desc))
    expect_message(out <- f(), "sorted rows by desc\\(cyl\\), desc\\(carb\\)")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"), desc)))

    # namespaced dplyr::desc as function arg
    f <- function() tidylog::arrange(mtcars, across(starts_with("c"), dplyr::desc))
    expect_message(out <- f(), "sorted rows by desc\\(cyl\\), desc\\(carb\\)")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"), dplyr::desc)))
})

test_that("arrange: pick", {

    # pick(everything())
    f <- function() tidylog::arrange(mtcars, pick(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}"))
    expect_equal(out, dplyr::arrange(mtcars, pick(everything())))

    # pick(starts_with())
    f <- function() tidylog::arrange(mtcars, pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, pick(starts_with("c"))))

    # pick(all_of())
    cols <- c("carb", "gear")
    f <- function() tidylog::arrange(mtcars, pick(all_of(cols)))
    expect_message(out <- f(), "sorted rows by carb, gear")
    expect_equal(out, dplyr::arrange(mtcars, pick(all_of(cols))))

    # pick(a, b) — multiple bare columns
    f <- function() tidylog::arrange(mtcars, pick(carb, gear))
    expect_message(out <- f(), "sorted rows by carb, gear")
    expect_equal(out, dplyr::arrange(mtcars, pick(carb, gear)))

    # pick(a:b) — range unpacks intermediate columns
    f <- function() tidylog::arrange(mtcars, pick(cyl:hp))
    expect_message(out <- f(), "sorted rows by cyl, disp, hp")
    expect_equal(out, dplyr::arrange(mtcars, pick(cyl:hp)))

    # pick(c(a, b))
    f <- function() tidylog::arrange(mtcars, pick(c(carb, gear)))
    expect_message(out <- f(), "sorted rows by carb, gear")
    expect_equal(out, dplyr::arrange(mtcars, pick(c(carb, gear))))

    # namespaced pick()
    f <- function() tidylog::arrange(mtcars, dplyr::pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::pick(starts_with("c"))))

    # empty pick() errors identically to dplyr
    expect_equal(
        tryCatch(dplyr::arrange(mtcars, pick()), error = conditionMessage),
        tryCatch(tidylog::arrange(mtcars, pick()), error = conditionMessage)
    )

    # pick(c()) — selects nothing, no changes
    f <- function() tidylog::arrange(mtcars, pick(c()))
    expect_message(out <- f(), "no changes")
    expect_equal(out, dplyr::arrange(mtcars, pick(c())))
})

test_that("arrange: mixed", {
    # mixed bare columns and across
    f <- function() tidylog::arrange(mtcars, carb, across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by carb, cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, carb, across(starts_with("c"))))

    # mixed bare columns and pick
    f <- function() tidylog::arrange(mtcars, carb, pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by carb, cyl, carb")
    expect_equal(out, dplyr::arrange(mtcars, carb, pick(starts_with("c"))))
})

test_that("arrange: NAs", {

    mtcars_na <- mtcars
    mtcars_na$carb[1] <- NA
    mtcars_na$gear[1] <- NA

    # single NA column reported
    f <- function() tidylog::arrange(mtcars_na, carb)
    expect_message(f(), "sorted rows by carb")
    expect_message(f(), "some columns contained NAs which sort last \\(carb\\)")

    # multiple NA columns reported
    f <- function() tidylog::arrange(mtcars_na, carb, gear)
    expect_message(f(), "some columns contained NAs which sort last \\(carb, gear\\)")

    # NA in desc() column still reported
    f <- function() tidylog::arrange(mtcars_na, desc(carb))
    expect_message(f(), "some columns contained NAs which sort last \\(carb\\)")

    # NA in across()-resolved column reported
    f <- function() tidylog::arrange(mtcars_na, across(starts_with("c")))
    expect_message(f(), "some columns contained NAs which sort last \\(carb\\)")

    # no NA note when no NAs present
    f <- function() tidylog::arrange(mtcars, carb)
    expect_message(out <- f(), "sorted rows by carb")
    expect_no_message(f(), message = "NAs")
})


test_that("arrange: edge cases and complex expressions", {
    # 1. Complex data-masking: should NOT trigger NA note even with NAs
    mtcars_with_nas <- mtcars |>
        dplyr::mutate(dplyr::across(mpg, \(x) na_if(x, 21.0)))
    f <- function() tidylog::arrange(mtcars_with_nas, mpg * 2)
    expect_message(out <- f(), "sorted rows by mpg \\* 2")
    expect_no_message(f(), message = "some columns contained NAs")
    expect_equal(out, dplyr::arrange(mtcars_with_nas, mpg * 2))

    f <- function() tidylog::arrange(mtcars_with_nas, cyl * hp)
    expect_message(out <- f(), "sorted rows by cyl \\* hp")
    expect_no_message(f(), message = "some columns contained NAs")
    expect_equal(out, dplyr::arrange(mtcars_with_nas, cyl * hp))

    # 2. .data pronoun
    f <- function() tidylog::arrange(mtcars, .data$carb)
    expect_message(out <- f(), "sorted rows by .data\\$carb")
    expect_equal(out, dplyr::arrange(mtcars, .data$carb))

    # 3. Non-syntactic names
    mtcars_with_nas_and_spaces <- mtcars_with_nas |>
        dplyr::rename("my mpg" = mpg)
    f <- function() tidylog::arrange(mtcars_with_nas_and_spaces, cyl, `my mpg`, hp)
    expect_message(out <- f(), message = "sorted rows by cyl, `my mpg`, hp")
    expect_message(f(), "some columns contained NAs which sort last \\(my mpg\\)")
    expect_equal(out, dplyr::arrange(mtcars_with_nas_and_spaces, cyl, `my mpg`, hp))
})
