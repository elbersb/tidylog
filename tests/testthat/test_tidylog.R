context("test_tidylog")
library("dplyr")
library("tidylog")

test_that("tidylog", {
    f <- function() tidylog::tidylog(mtcars)
    expect_message(f(), regexp = "data\\.frame.*32 rows.*11 columns")

    f <- function() tidylog::tidylog(as_tibble(mtcars))
    expect_message(f(), regexp = "tibble.*32 rows.*11 columns")

    f <- function() tidylog::tidylog(group_by(mtcars, cyl, mpg))
    expect_message(f(), regexp = "grouped tibble.*32 rows.*11 columns")
})

test_that("logging on/off", {
    options("tidylog.display" = list())
    expect_silent(tidylog::filter(mtcars, mpg > 20))
    expect_silent(tidylog::select(mtcars, mpg))
    expect_silent(tidylog::group_by(mtcars, mpg))
    expect_silent(tidylog::left_join(dplyr::band_members, dplyr::band_instruments, by = "name"))
    expect_silent(tidylog::mutate(mtcars, test = TRUE))
    expect_silent(tidylog::summarize(mtcars, test = TRUE))
    expect_silent(tidylog::gather(mtcars))
    expect_silent(tidylog::spread(mtcars, cyl, hp))
    expect_silent(tidylog::tidylog(mtcars))

    # warnings
    options("tidylog.display" = "x")
    expect_warning(filter(mtcars, mpg > 20))

    options("tidylog.display" = list("x", message))
    expect_warning(filter(mtcars, mpg > 20))

    # back to default
    options("tidylog.display" = NULL)
    expect_message(filter(mtcars, mpg > 20))
})
