library("dplyr")
library("tidyr")
library("tidylog")
context("test_longer_wider")

test_that("pivot_longer", {
    mtcars$id <- seq_len(nrow(mtcars))

    expect_message({
        outlog <- tidylog::pivot_longer(mtcars, -id, names_to = "var", values_to = "value")
    })
    expect_equal(nrow(outlog), 352)
    expect_equal(ncol(outlog), 3)

    expect_silent({
        outtidy <- tidyr::pivot_longer(mtcars, -id, names_to = "var", values_to = "value")
    })

    expect_equal(outlog, outtidy)

})

test_that("pivot_longer: argument order", {
    mtcars$id <- seq_len(nrow(mtcars))

    expect_message({
        out_ab <- tidylog::pivot_longer(names_to = "var", values_to = "value",
            .data = mtcars, cols = -id)
    })

    expect_message({
        out_ba <- tidylog::pivot_longer(.data = mtcars, cols = -id,
            names_to = "var", values_to = "value")
    })

    expect_equal(out_ab, out_ba)
})

test_that("pivot_wider", {
    expect_message({
        outlog <- tidylog::pivot_wider(mtcars, names_from = vs, values_from = cyl)
    })
    expect_equal(nrow(outlog), 32)
    expect_equal(ncol(outlog), 11)

    expect_silent({
        outtidyr <- tidyr::pivot_wider(mtcars, names_from = vs, values_from = cyl)
    })

    expect_equal(outlog, outtidyr)
})

test_that("pivot_wider: argument order", {
    expect_message({
        out_ab <- tidylog::pivot_wider(names_from = vs, values_from = cyl, .data = mtcars)
    })

    expect_message({
        out_ba <- tidylog::pivot_wider(mtcars, names_from = vs, values_from = cyl)
    })

    expect_equal(out_ab, out_ba)
})

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

test_that("spread", {
    expect_message({
        outlog <- tidylog::spread(mtcars, hp, carb)
    })
    expect_equal(nrow(outlog), 32)
    expect_equal(ncol(outlog), 31)

    expect_silent({
        outtidyr <- tidyr::spread(mtcars, hp, carb)
    })

    expect_equal(outlog, outtidyr)

})

test_that("spread: argument order", {
    expect_message({
        out_ab <- tidylog::spread(.data = mtcars, hp, carb)
    })

    expect_message({
        out_ba <- tidylog::spread(hp, carb, .data = mtcars)
    })

    expect_equal(out_ab, out_ba)
})
