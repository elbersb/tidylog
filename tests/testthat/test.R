library("dplyr")
library("tidylog")
context("test_filter")

test_that("filter", {
    expect_equal(nrow(filter(mtcars, gear > 3)), 17)
})
