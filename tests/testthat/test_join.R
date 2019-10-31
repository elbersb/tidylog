context("test_group_by")
library("dplyr")
library("tidylog")

test_that("join", {

    expect_message({
        out <- tidylog::inner_join(dplyr::band_members,
                                   dplyr::band_instruments,
                                   by = "name")
    })
    expect_equal(out, dplyr::inner_join(dplyr::band_members,
                                        dplyr::band_instruments,
                                        by = "name"))

    expect_message({
        out <- tidylog::full_join(dplyr::band_members,
                                  dplyr::band_instruments,
                                  by = "name")
    })
    expect_equal(out, dplyr::full_join(dplyr::band_members,
                                       dplyr::band_instruments,
                                       by = "name"))

    expect_message({
        out <- tidylog::left_join(dplyr::band_members,
                                  dplyr::band_instruments,
                                  by = "name")
    })
    expect_equal(out, dplyr::left_join(dplyr::band_members,
                                       dplyr::band_instruments,
                                       by = "name"))

    expect_message({
        out <- tidylog::right_join(dplyr::band_members,
                                   dplyr::band_instruments,
                                   by = "name")
    })
    expect_equal(out, dplyr::right_join(dplyr::band_members,
                                        dplyr::band_instruments,
                                        by = "name"))

    expect_message({
        out <- tidylog::anti_join(dplyr::band_members,
                                  dplyr::band_instruments,
                                  by = "name")
    })
    expect_equal(out, dplyr::anti_join(dplyr::band_members,
                                       dplyr::band_instruments,
                                       by = "name"))

    expect_message({
        out <- tidylog::semi_join(dplyr::band_members,
                                  dplyr::band_instruments,
                                  by = "name")
    })
    expect_equal(out, dplyr::semi_join(dplyr::band_members,
                                       dplyr::band_instruments,
                                       by = "name"))

    expect_silent({
        out <- dplyr::inner_join(dplyr::band_members,
                                 dplyr::band_instruments,
                                 by = "name")
        out <- dplyr::full_join(dplyr::band_members,
                                dplyr::band_instruments,
                                by = "name")
        out <- dplyr::left_join(dplyr::band_members,
                                dplyr::band_instruments,
                                by = "name")
        out <- dplyr::right_join(dplyr::band_members,
                                 dplyr::band_instruments,
                                 by = "name")
        out <- dplyr::anti_join(dplyr::band_members,
                                dplyr::band_instruments,
                                by = "name")
        out <- dplyr::semi_join(dplyr::band_members,
                                dplyr::band_instruments,
                                by = "name")
    })
})

test_that("join: argument order", {
    expect_message({
        out <- tidylog::inner_join(y = dplyr::band_instruments,
                                   x = dplyr::band_members)
    })
    expect_equal(out, dplyr::inner_join(y = dplyr::band_instruments,
                                        x = dplyr::band_members))
    expect_equal(out, dplyr::inner_join(x = dplyr::band_members,
                                        y = dplyr::band_instruments))

    expect_message({
        out <- tidylog::inner_join(by = "name",
                                   x = dplyr::band_members,
                                   y = dplyr::band_instruments)
    })
    expect_equal(out, dplyr::inner_join(by = "name",
                                        x = dplyr::band_members,
                                        y = dplyr::band_instruments))
})

# adapted from dplyr tests

a <- data.frame(x = c(1, 1, 2, 3), y = 1:4)
b <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
    msg <- capture_messages({
        j <- tidylog::inner_join(a, b, "x")
    })

    # one row from x not included
    expect_match(msg, "rows only in x\\s*\\(1\\)", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in y\\s*\\(1\\)", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate semi join has x columns, matching rows", {
    msg <- capture_messages({
        j <- tidylog::semi_join(a, b, "x")
    })

    # one row from x not included
    expect_match(msg, "rows only in x\\s*\\(1\\)", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in y\\s*\\(1\\)", all = FALSE)
    # three rows matched, no duplication
    expect_match(msg, "matched rows\\s*3", all = FALSE)
    expect_true(!any(grepl("duplicate", msg)))
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate full join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::full_join(a, b, "x")
    })

    # one row from x included
    expect_match(msg, "rows only in x\\s*1", all = FALSE)
    # one row from y included
    expect_match(msg, "rows only in y\\s*1", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate left join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::left_join(a, b, "x")
    })

    # one row from x included
    expect_match(msg, "rows only in x\\s*1", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in y\\s*\\(1\\)", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate right join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::right_join(a, b, "x")
    })

    # one row from x not included
    expect_match(msg, "rows only in x\\s*\\(1\\)", all = FALSE)
    # one row from y included
    expect_match(msg, "rows only in y\\s*1", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate anti join has x columns, missing rows", {
    msg <- capture_messages({
        j <- tidylog::anti_join(a, b, "x")
    })

    # one row from x included
    expect_match(msg, "rows only in x\\s*1", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in y\\s*\\(1\\)", all = FALSE)
    # three matched rows not included
    expect_match(msg, "matched rows\\s*\\(3\\)", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

# edge cases for duplication

test_that("no duplication", {
    a <- data.frame(x = c(1, 2, 3, 5), y = 1:4)
    b <- data.frame(x = c(1, 2, 3, 4), z = 1:4)

    msg <- capture_messages(tidylog::inner_join(a, b, "x"))
    expect_true(!any(grepl("duplicate", msg)))
    msg <- capture_messages(tidylog::full_join(a, b, "x"))
    expect_true(!any(grepl("duplicate", msg)))
    msg <- capture_messages(tidylog::left_join(a, b, "x"))
    expect_true(!any(grepl("duplicate", msg)))
})
