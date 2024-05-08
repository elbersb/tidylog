library("dplyr")
library("tidyr")
library("tidylog")
context("test_longer_wider")

mtcars_with_id <- dplyr::mutate(mtcars, id = seq_len(dplyr::n()))
mtcars_with_name <- tidyr::as_tibble(mtcars, rownames = "name")

test_tidyr_fun <- function(fun_name, tidylog_fun, tidyr_fun, data, ..., expected_nrow, expected_ncol) {
    # Test that tidylog output matches tidyr.
    test_that(fun_name, {
        expect_message({
            outlog <- tidylog_fun(data, ...)
        })
        expect_equal(nrow(outlog), expected_nrow)
        expect_equal(ncol(outlog), expected_ncol)

        expect_silent({
            outtidy <- tidyr_fun(data, ...)
        })

        expect_equal(outlog, outtidy)
    })

    # Test that argument order doesn't matter.
    test_that(glue::glue("{fun_name}: argument order"), {
        expect_message({
            out_ab <- tidylog_fun(data = data, ...)
        })

        expect_message({
            out_ba <- tidylog_fun(..., data = data)
        })

        expect_equal(out_ab, out_ba)
    })
}


test_tidyr_fun(
    "pivot_longer", tidylog::pivot_longer, tidyr::pivot_longer,
    mtcars_with_id, -id, names_to = "var", values_to = "value",
    expected_nrow = 352, expected_ncol = 3
)


test_tidyr_fun(
    "pivot_wider", tidylog::pivot_wider, tidyr::pivot_wider,
    mtcars, names_from = vs, values_from = cyl,
    expected_nrow = 32, expected_ncol = 11
)

test_tidyr_fun(
    "gather", tidylog::gather, tidyr::gather,
    mtcars,
    expected_nrow = 352, expected_ncol = 2
)

test_tidyr_fun(
    "spread", tidylog::spread, tidyr::spread,
    mtcars, hp, carb,
    expected_nrow = 32, expected_ncol = 31
)

test_tidyr_fun(
    "separate_wider_delim", tidylog::separate_wider_delim, tidyr::separate_wider_delim,
    mtcars_with_name, name, " ", names=c("A", "B"),
    too_few = "align_start", too_many = "merge",
    expected_nrow = 32, expected_ncol = 13
)

test_tidyr_fun(
    "separate_wider_position", tidylog::separate_wider_position, tidyr::separate_wider_position,
    mtcars_with_name, name, c("A" = 3, "B" = 4),
    too_few = "align_start", too_many = "drop",
    expected_nrow = 32, expected_ncol = 13
)

test_tidyr_fun(
    "separate_wider_regex", tidylog::separate_wider_regex, tidyr::separate_wider_regex,
    mtcars_with_name, name, c("A" = "\\w+", "\\s", "B" = ".*"),
    too_few = "align_start",
    expected_nrow = 32, expected_ncol = 13
)
