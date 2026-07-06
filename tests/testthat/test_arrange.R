context("test_arrange")
suppressWarnings(library("dplyr"))
suppressWarnings(library("tidylog"))

.ellipsis <- cli::symbol$ellipsis

# Column contained NAs
sup_1 <- cli::symbol$sup_1
na_regex <- paste0(sup_1, "contains NAs, which sort to the end$")

# Column's NA status is unknown
sup_2 <- cli::symbol$sup_2
unknown_regex <- paste0(sup_2, "NA status unknown$")

# Combined:
combined_regex <- paste0(sub("\\$", "", na_regex), "; ", unknown_regex)

test_that("arrange: basic", {

    # empty has no effect and does not error
    f <- function() tidylog::arrange(mtcars)
    expect_message(out <- f(), "no changes$")
    expect_equal(out, dplyr::arrange(mtcars))

    # no changes because already sorted
    input <- dplyr::arrange(mtcars, carb)
    f <- function() tidylog::arrange(input, carb)
    expect_message(out <- f(), "no changes$")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # single column
    f <- function() tidylog::arrange(mtcars, carb)
    expect_message(out <- f(), "sorted rows by carb$")
    expect_equal(out, dplyr::arrange(mtcars, carb))

    # multiple columns
    f <- function() tidylog::arrange(mtcars, carb, gear, mpg)
    expect_message(out <- f(), "sorted rows by carb, gear, mpg$")
    expect_equal(out, dplyr::arrange(mtcars, carb, gear, mpg))

    # >5 columns only shows the first 5
    f <- function() tidylog::arrange(mtcars, mpg, cyl, disp, hp, drat, wt)
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}$"))
    expect_equal(out, dplyr::arrange(mtcars, mpg, cyl, disp, hp, drat, wt))
})

test_that("arrange: bare desc", {

    # single desc()
    f <- function() tidylog::arrange(mtcars, desc(carb))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\)$")
    expect_equal(out, dplyr::arrange(mtcars, desc(carb)))

    # all desc()
    f <- function() tidylog::arrange(mtcars, desc(carb), desc(gear))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\), desc\\(gear\\)$")
    expect_equal(out, dplyr::arrange(mtcars, desc(carb), desc(gear)))

    # mixed bare and desc()
    f <- function() tidylog::arrange(mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(), "sorted rows by carb, desc\\(gear\\), mpg$")
    expect_equal(out, dplyr::arrange(mtcars, carb, desc(gear), mpg))

    # namespaced dplyr::desc()
    f <- function() tidylog::arrange(mtcars, dplyr::desc(carb))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\)$")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::desc(carb)))
})

test_that("arrange: grouped", {
    grp_mtcars <- dplyr::group_by(mtcars, cyl)

    # grouped sorting
    f <- function() tidylog::arrange(grp_mtcars, carb, desc(gear), mpg)
    expect_message(out <- f(),
                   "\\(grouped\\): sorted rows within groups by carb, desc\\(gear\\), mpg$")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, desc(gear), mpg))
})

test_that("arrange excludes named arguments", {
    grp_mtcars <- dplyr::group_by(mtcars, cyl)

    # .by_group is excluded from display
    f <- function() tidylog::arrange(grp_mtcars, carb, .by_group = TRUE)
    expect_message(out <- f(), "sorted rows within groups by carb$")
    expect_equal(out, dplyr::arrange(grp_mtcars, carb, .by_group = TRUE))

    # .locale is excluded from display
    f <- function() tidylog::arrange(mtcars, carb, .locale = "en")
    expect_message(out <- f(), "sorted rows by carb$")
    expect_equal(out, dplyr::arrange(mtcars, carb, .locale = "en"))
})

test_that("arrange: programmatic", {

    # !!sym() is evaluated to the column name
    col <- "carb"
    f <- function() tidylog::arrange(mtcars, !!sym(col))
    expect_message(out <- f(), "sorted rows by carb$")
    expect_equal(out, dplyr::arrange(mtcars, !!sym(col)))
})

test_that("arrange: across", {

    # across(everything())
    f <- function() tidylog::arrange(mtcars, across(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}$"))
    expect_equal(out, dplyr::arrange(mtcars, across(everything())))

    # across(starts_with())
    f <- function() tidylog::arrange(mtcars, across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb$")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"))))

    # across(all_of())
    cols <- c("carb", "gear")
    f <- function() tidylog::arrange(mtcars, across(all_of(cols)))
    expect_message(out <- f(), "sorted rows by carb, gear$")
    expect_equal(out, dplyr::arrange(mtcars, across(all_of(cols))))

    # across(c(a, b))
    f <- function() tidylog::arrange(mtcars, across(c(carb, gear)))
    expect_message(out <- f(), "sorted rows by carb, gear$")
    expect_equal(out, dplyr::arrange(mtcars, across(c(carb, gear))))

    # across(a:b) unpacks intermediate columns
    f <- function() tidylog::arrange(mtcars, across(cyl:hp))
    expect_message(out <- f(), "sorted rows by cyl, disp, hp$")
    expect_equal(out, dplyr::arrange(mtcars, across(cyl:hp)))

    # across(-condition) unpacks inverse correctly
    f <- function() tidylog::arrange(mtcars, across(-ends_with("p")))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, drat, wt, qsec, {.ellipsis}$"))
    expect_equal(out, dplyr::arrange(mtcars, across(-ends_with("p"))))

    # namespaced across()
    f <- function() tidylog::arrange(mtcars, dplyr::across(ends_with("p")))
    expect_message(out <- f(), "sorted rows by disp, hp$")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::across(ends_with("p"))))

    # across() matching no columns — no changes
    f <- function() tidylog::arrange(mtcars, across(starts_with("zzz")))
    expect_message(out <- f(), "no changes$")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("zzz"))))

    # empty across() treated as everything() and transmits dplyr deprecation warning
    f <- function() tidylog::arrange(mtcars, across())
    expect_warning(
        expect_message(
            out <- f(),
            glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}$")
        )
    )

    suppressWarnings(
        expect_equal(out, dplyr::arrange(mtcars, across()))
    )

    # across(c()) — selects nothing, no changes
    f <- function() tidylog::arrange(mtcars, across(c()))
    expect_message(out <- f(), "no changes$")
    expect_equal(out, dplyr::arrange(mtcars, across(c())))
})

test_that("arrange: across with desc", {

    # desc as function arg
    f <- function() tidylog::arrange(mtcars, across(starts_with("c"), desc))
    expect_message(out <- f(), "sorted rows by desc\\(cyl\\), desc\\(carb\\)$")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"), desc)))

    # namespaced dplyr::desc as function arg
    f <- function() tidylog::arrange(mtcars, across(starts_with("c"), dplyr::desc))
    expect_message(out <- f(), "sorted rows by desc\\(cyl\\), desc\\(carb\\)$")
    expect_equal(out, dplyr::arrange(mtcars, across(starts_with("c"), dplyr::desc)))
})

test_that("arrange: pick", {

    # pick(everything())
    f <- function() tidylog::arrange(mtcars, pick(everything()))
    expect_message(out <- f(),
                   glue::glue("sorted rows by mpg, cyl, disp, hp, drat, {.ellipsis}$"))
    expect_equal(out, dplyr::arrange(mtcars, pick(everything())))

    # pick(starts_with())
    f <- function() tidylog::arrange(mtcars, pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb$")
    expect_equal(out, dplyr::arrange(mtcars, pick(starts_with("c"))))

    # pick(all_of())
    cols <- c("carb", "gear")
    f <- function() tidylog::arrange(mtcars, pick(all_of(cols)))
    expect_message(out <- f(), "sorted rows by carb, gear$")
    expect_equal(out, dplyr::arrange(mtcars, pick(all_of(cols))))

    # pick(a, b) — multiple bare columns
    f <- function() tidylog::arrange(mtcars, pick(carb, gear))
    expect_message(out <- f(), "sorted rows by carb, gear$")
    expect_equal(out, dplyr::arrange(mtcars, pick(carb, gear)))

    # pick(a:b) — range unpacks intermediate columns
    f <- function() tidylog::arrange(mtcars, pick(cyl:hp))
    expect_message(out <- f(), "sorted rows by cyl, disp, hp$")
    expect_equal(out, dplyr::arrange(mtcars, pick(cyl:hp)))

    # pick(c(a, b))
    f <- function() tidylog::arrange(mtcars, pick(c(carb, gear)))
    expect_message(out <- f(), "sorted rows by carb, gear$")
    expect_equal(out, dplyr::arrange(mtcars, pick(c(carb, gear))))

    # namespaced pick()
    f <- function() tidylog::arrange(mtcars, dplyr::pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by cyl, carb$")
    expect_equal(out, dplyr::arrange(mtcars, dplyr::pick(starts_with("c"))))

    # empty pick() errors identically to dplyr
    expect_equal(
        tryCatch(dplyr::arrange(mtcars, pick()), error = conditionMessage),
        tryCatch(tidylog::arrange(mtcars, pick()), error = conditionMessage)
    )

    # pick(c()) — selects nothing, no changes
    f <- function() tidylog::arrange(mtcars, pick(c()))
    expect_message(out <- f(), "no changes$")
    expect_equal(out, dplyr::arrange(mtcars, pick(c())))
})

test_that("arrange: mixed", {
    # mixed bare columns and across
    f <- function() tidylog::arrange(mtcars, carb, across(starts_with("c")))
    expect_message(out <- f(), "sorted rows by carb, cyl$")
    expect_equal(out, dplyr::arrange(mtcars, carb, across(starts_with("c"))))

    # mixed bare columns and pick
    f <- function() tidylog::arrange(mtcars, carb, pick(starts_with("c")))
    expect_message(out <- f(), "sorted rows by carb, cyl$")
    expect_equal(out, dplyr::arrange(mtcars, carb, pick(starts_with("c"))))
})

test_that("arrange: complex desc", {
    # desc(across()) expands and wraps each column in desc()
    f <- function() tidylog::arrange(mtcars, desc(across(starts_with("c"))))
    expect_message(out <- f(), "sorted rows by desc\\(cyl\\), desc\\(carb\\)$")
    expect_equal(out, dplyr::arrange(mtcars, desc(across(starts_with("c")))))

    # desc(pick()) expands and wraps each column in desc()
    f <- function() tidylog::arrange(mtcars, desc(pick(carb, gear)))
    expect_message(out <- f(), "sorted rows by desc\\(carb\\), desc\\(gear\\)$")
    expect_equal(out, dplyr::arrange(mtcars, desc(pick(carb, gear))))

    # desc(col * 2) — complex expression, shown as-is, without backticking.
    f <- function() tidylog::arrange(mtcars, desc(mpg * 2))
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by desc\\(mpg \\* 2\\){sup_2}$")
    ) |>
        expect_message(message = unknown_regex)
    expect_equal(out, dplyr::arrange(mtcars, desc(mpg * 2)))
})

test_that("arrange: NAs", {

    mtcars_na <- mtcars
    mtcars_na$carb[1] <- NA
    mtcars_na$gear[1] <- NA

    # single NA column reported
    f <- function() tidylog::arrange(mtcars_na, carb)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by carb{sup_1}$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, carb))

    # multiple NA columns reported
    f <- function() tidylog::arrange(mtcars_na, carb, gear)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by carb{sup_1}, gear{sup_1}$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, carb, gear))

    # non-NA columns have no marker
    f <- function() tidylog::arrange(mtcars_na, carb, gear, cyl)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by carb{sup_1}, gear{sup_1}, cyl$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, carb, gear, cyl))

    # NA in desc() column still reported
    f <- function() tidylog::arrange(mtcars_na, desc(carb))
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by desc\\(carb\\){sup_1}$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, desc(carb)))

    # NA in across()-resolved column reported
    f <- function() tidylog::arrange(mtcars_na, across(starts_with("c")))
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by cyl, carb{sup_1}$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, across(starts_with("c"))))

    # no NA note when no NAs present
    f <- function() tidylog::arrange(mtcars, carb)
    expect_message(
        out <- f(),
        message = "sorted rows by carb$"
    ) |>
        expect_no_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars, carb))

})

test_that("arrange: edge cases and complex expressions", {

    mtcars_na <- mtcars
    mtcars_na$mpg[1] <- NA

    # 1. Complex data-masking:
    #   A. Should NOT trigger sup_1 NA note even with NA
    #   B. Should be back-ticked when not within desc().
    f <- function() tidylog::arrange(mtcars_na, mpg * 2)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by `mpg \\* 2`{sup_2}$")
    ) |>
        expect_no_message(message = na_regex) |>
        expect_message(message = unknown_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, mpg * 2))

    f <- function() tidylog::arrange(mtcars_na, cyl * hp)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by `cyl \\* hp`{sup_2}$")
    ) |>
        expect_no_message(message = na_regex) |>
        expect_message(message = unknown_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, cyl * hp))

    # 2A: .data pronoun without NA
    f <- function() tidylog::arrange(mtcars, .data$carb)
    expect_message(out <- f(), "sorted rows by carb$")
    expect_equal(out, dplyr::arrange(mtcars, .data$carb))

    # 2B: .data pronoun with NA
    f <- function() tidylog::arrange(mtcars_na, .data$mpg)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by mpg{sup_1}$"),
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, .data$mpg))

    # 3. Non-syntactic names
    mtcars_na_space <- dplyr::rename(mtcars_na, "my mpg" = mpg)
    f <- function() tidylog::arrange(mtcars_na_space, cyl, `my mpg`, hp)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by cyl, `my mpg`{sup_1}, hp$")
    ) |>
        expect_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na_space, cyl, `my mpg`, hp))

    # 4. Repeated labels with qualifiers are maintained
    f <- function() tidylog::arrange(mtcars, carb, desc(carb), carb * 2)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by carb, desc\\(carb\\), `carb \\* 2`{sup_2}$"),
    ) |>
        expect_message(message = unknown_regex)
    expect_equal(out, dplyr::arrange(mtcars, carb, desc(carb), carb * 2))
})

test_that("arrange: NA and complex expressions combined", {

    mtcars_na <- mtcars
    mtcars_na$mpg[1] <- NA


    # 1. Present on one line
    f <- function() tidylog::arrange(mtcars_na, mpg, mpg * 2)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by mpg{sup_1}, `mpg \\* 2`{sup_2}$")
    ) |>
        expect_message(message = combined_regex) |>
        # Can't find just the NA part anchored to the end
        expect_no_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, mpg, mpg * 2))

    # 2. The order of the variables does not change the order of the NA message
    f <- function() tidylog::arrange(mtcars_na, mpg * 2, mpg)
    expect_message(
        out <- f(),
        message = glue::glue("sorted rows by `mpg \\* 2`{sup_2}, mpg{sup_1}$")
    ) |>
        expect_message(message = combined_regex) |>
        # Can't find just the NA part anchored to the end
        expect_no_message(message = na_regex)
    expect_equal(out, dplyr::arrange(mtcars_na, mpg * 2, mpg))
})
