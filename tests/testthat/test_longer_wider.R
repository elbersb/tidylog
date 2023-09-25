library("dplyr")
library("tidyr")
library("tidylog")
context("test_longer_wider")

mtcars_with_id <- dplyr::mutate(mtcars, id = seq_len(dplyr::n()))
mtcars_with_name <- tidyr::as_tibble(mtcars, rownames = "name")

test_that_tidylog_matches_tidyr <- function(fun_name, tidylog_fun, tidyr_fun, ..., expected_nrow, expected_ncol) {
    test_that(fun_name, {
        expect_message({
            outlog <- tidylog_fun(...)
        })
        expect_equal(nrow(outlog), expected_nrow)
        expect_equal(ncol(outlog), expected_ncol)

        expect_silent({
            outtidy <- tidyr_fun(...)
        })

        expect_equal(outlog, outtidy)
    })
}

test_argument_order <- function(fun_name, tidylog_fun, data, ...) {
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

test_that_tidylog_matches_tidyr(
    "pivot_longer", tidylog::pivot_longer, tidyr::pivot_longer,
    mtcars_with_id, -id, names_to = "var", values_to = "value",
    expected_nrow = 352, expected_ncol = 3
)

test_argument_order(
    "pivot_longer", tidylog::pivot_longer,
    data = mtcars_with_id, cols = -id, names_to = "var", values_to = "value"
)

test_that_tidylog_matches_tidyr(
    "pivot_wider", tidylog::pivot_wider, tidyr::pivot_wider,
    mtcars, names_from = vs, values_from = cyl,
    expected_nrow = 32, expected_ncol = 11
)

test_argument_order(
    "pivot_wider", tidylog::pivot_wider,
    mtcars, names_from = vs, values_from = cyl
)

test_that_tidylog_matches_tidyr(
    "gather", tidylog::gather, tidyr::gather,
    mtcars,
    expected_nrow = 352, expected_ncol = 2
)

test_argument_order(
    "gather", tidylog::gather,
    mtcars, carb
)

test_that_tidylog_matches_tidyr(
    "spread", tidylog::spread, tidyr::spread,
    mtcars, hp, carb,
    expected_nrow = 32, expected_ncol = 31
)

test_argument_order(
    "spread", tidylog::spread,
    mtcars, hp, carb
)
