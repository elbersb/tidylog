library("dplyr")
library("tidylog")
context("test_gather")

test_that("gather", {
    expect_message({
        outlog <- tidylog::gather(mtcars)
    })
    expect_equal(nrow(outlog), 352)
    expect_equal(ncol(outlog), 2)

    expect_silent({
        outtidy <- tidyr::gather(mtcars)
    })

    expect_equal(outlog, outtidy)

})

test_that("gather: argument order", {
    expect_message({
        out_ab <- tidylog::gather(.data = mtcars, carb)
    })

    expect_message({
        out_ba <- tidylog::gather(carb, .data = mtcars)
    })

    expect_equal(out_ab, out_ba)
})
