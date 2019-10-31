context("test_ungroup")

test_that("ungroup", {
    expect_message({
        out <- tidylog::ungroup(mtcars, mpg)
    })
    expect_equal(dplyr::is.grouped_df(out), FALSE)

    expect_silent({
        out <- dplyr::ungroup(mtcars, mpg)
    })
})
