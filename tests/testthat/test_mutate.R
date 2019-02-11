library("dplyr")
library("tidylog")
context("test_mutate")

test_that("mutate", {
    expect_message({
        out <- tidylog::mutate(mtcars, test = TRUE)
    })
    expect_equal(all(out$test), TRUE)
})

test_that("transmute", {
    expect_message({
        out <- tidylog::transmute(mtcars, test = TRUE)
    })
    expect_equal(all(out$test), TRUE)
    expect_equal(ncol(out), 1)
})

test_that("mutate: scoped variants", {

})

test_that("transmute: scoped variants", {

})

test_that("mutate: argument order", {
    expect_message({
        out <- tidylog::mutate(test = TRUE, .data = mtcars)
    })
    expect_equal(all(out$test), TRUE)
})

test_that("transmute: argument order", {
    expect_message({
        out <- tidylog::transmute(test = TRUE, .data = mtcars)
    })
    expect_equal(all(out$test), TRUE)
    expect_equal(ncol(out), 1)
})
