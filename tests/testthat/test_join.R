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
        j <- tidylog::inner_join(a, b, "x", relationship = "many-to-many")
    })

    # one row from x not included
    expect_match(msg, "rows only in a\\s*\\(1\\)", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in b\\s*\\(1\\)", all = FALSE)
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
    expect_match(msg, "rows only in a\\s*\\(1\\)", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in b\\s*\\(1\\)", all = FALSE)
    # three rows matched, no duplication
    expect_match(msg, "matched rows\\s*3", all = FALSE)
    expect_true(!any(grepl("duplicate", msg)))
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate full join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::full_join(a, b, "x", relationship = "many-to-many")
    })

    # one row from x included
    expect_match(msg, "rows only in a\\s*1", all = FALSE)
    # one row from y included
    expect_match(msg, "rows only in b\\s*1", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate left join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::left_join(a, b, "x", relationship = "many-to-many")
    })

    # one row from x included
    expect_match(msg, "rows only in a\\s*1", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in b\\s*\\(1\\)", all = FALSE)
    # four rows matched, including duplicates
    expect_match(msg, "matched rows\\s*4", all = FALSE)
    expect_match(msg, "duplicate", all = FALSE)
    # total rows
    expect_match(msg, paste0("rows total\\s*", nrow(j)), all = FALSE)
})

test_that("univariate right join has all columns, all rows", {
    msg <- capture_messages({
        j <- tidylog::right_join(a, b, "x", relationship = "many-to-many")
    })

    # one row from x not included
    expect_match(msg, "rows only in a\\s*\\(1\\)", all = FALSE)
    # one row from y included
    expect_match(msg, "rows only in b\\s*1", all = FALSE)
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
    expect_match(msg, "rows only in a\\s*1", all = FALSE)
    # one row from y not included
    expect_match(msg, "rows only in b\\s*\\(1\\)", all = FALSE)
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

get_indices <- function(msg) {
    matched <- gregexpr(pattern =',', msg[c(2, 3, 4, 6)])
    unique(sapply(matched, function(x) x[[1]]))
}

test_that("correct alignment", {
    msg <- function(fun) {
        capture_messages({
            j <- fun(nycflights13::flights, nycflights13::weather,
                by = c("year", "month", "day", "origin", "hour", "time_hour"))
        })
    }

    expect_equal(length(get_indices(msg(inner_join))), 1)
    expect_equal(length(get_indices(msg(semi_join))), 1)
    expect_equal(length(get_indices(msg(left_join))), 1)
    expect_equal(length(get_indices(msg(right_join))), 1)
    expect_equal(length(get_indices(msg(full_join))), 1)
    expect_equal(length(get_indices(msg(anti_join))), 1)
})


test_that("correct alignment -- long df name", {
    msg <- function(fun) {
        capture_messages({
            verylongnameverylongnameverylongname <- nycflights13::flights
            j <- fun(verylongnameverylongnameverylongname, nycflights13::weather,
                by = c("year", "month", "day", "origin", "hour", "time_hour"))
        })
    }

    expect_equal(length(get_indices(msg(inner_join))), 1)
    expect_equal(length(get_indices(msg(semi_join))), 1)
    expect_equal(length(get_indices(msg(left_join))), 1)
    expect_equal(length(get_indices(msg(right_join))), 1)
    expect_equal(length(get_indices(msg(full_join))), 1)
    expect_equal(length(get_indices(msg(anti_join))), 1)

    msg <- function(fun) {
        capture_messages({
            verylongnameverylongnameverylongname <- nycflights13::weather
            j <- fun(nycflights13::flights, verylongnameverylongnameverylongname,
                by = c("year", "month", "day", "origin", "hour", "time_hour"))
        })
    }

    expect_equal(length(get_indices(msg(inner_join))), 1)
    expect_equal(length(get_indices(msg(semi_join))), 1)
    expect_equal(length(get_indices(msg(left_join))), 1)
    expect_equal(length(get_indices(msg(right_join))), 1)
    expect_equal(length(get_indices(msg(full_join))), 1)
    expect_equal(length(get_indices(msg(anti_join))), 1)
})

test_that("join_by with `==` generates same messages as standard `by`", {
    expect_equal(
        capture_messages(inner_join(dplyr::band_members, dplyr::band_instruments,
                                    by = "name")),
        capture_messages(inner_join(dplyr::band_members, dplyr::band_instruments,
                                    by = join_by(name))),
    )
    expect_equal(
        capture_messages(semi_join(dplyr::band_members, dplyr::band_instruments,
                                   by = "name")),
        capture_messages(semi_join(dplyr::band_members, dplyr::band_instruments,
                                   by = join_by(name))),
    )
    expect_equal(
        capture_messages(left_join(dplyr::band_members, dplyr::band_instruments,
                                   by = "name")),
        capture_messages(left_join(dplyr::band_members, dplyr::band_instruments,
                                   by = join_by(name))),
    )
    expect_equal(
        capture_messages(right_join(dplyr::band_members, dplyr::band_instruments,
                                    by = "name")),
        capture_messages(right_join(dplyr::band_members, dplyr::band_instruments,
                                    by = join_by(name))),
    )
    expect_equal(
        capture_messages(full_join(dplyr::band_members, dplyr::band_instruments,
                                   by = "name")),
        capture_messages(full_join(dplyr::band_members, dplyr::band_instruments,
                                   by = join_by(name))),
    )
    expect_equal(
        capture_messages(anti_join(dplyr::band_members, dplyr::band_instruments,
                                   by = "name")),
        capture_messages(anti_join(dplyr::band_members, dplyr::band_instruments,
                                   by = join_by(name))),
    )

})


test_that("join_by with >= succeeds despite not giving full message", {
    band_members_with_birth <- dplyr::mutate(dplyr::band_members,
                                             birth_year = c(1943, 1940, 1942))
    band_instruments_with_death <- dplyr::mutate(dplyr::band_instruments,
                                                 death_year = c(NA, 1980, NA))

    # ---- semi_join ----
    expect_silent({
        dpout <- dplyr::semi_join(
            band_members_with_birth,
            band_instruments_with_death,
            by = dplyr::join_by(name, birth_year < death_year)
        )
    })

    msgs <- capture_messages({
        tlout <- semi_join(
            band_members_with_birth,
            band_instruments_with_death,
            by = dplyr::join_by(name, birth_year < death_year)
        )
    })
    expect_equal(dpout, tlout)

    # Expect a columns message and a rows message.
    expect_equal(length(msgs), 2)
    expect_equal(msgs[1], "semi_join: added no columns")
    expect_equal(msgs[2], "semi_join: removed 2 rows (67%), one row remaining")


    # ---- anti_join ----
    expect_silent({
        dpout <- dplyr::anti_join(
            band_members_with_birth,
            band_instruments_with_death,
            by = dplyr::join_by(name, birth_year < death_year)
        )
    })

    msgs <- capture_messages({
        tlout <- anti_join(
            band_members_with_birth,
            band_instruments_with_death,
            by = dplyr::join_by(name, birth_year < death_year)
        )
    })
    expect_equal(dpout, tlout)

    # Expect a columns message and a rows message.
    expect_equal(length(msgs), 2)
    expect_equal(msgs[1], "anti_join: added no columns")
    expect_equal(msgs[2], "anti_join: removed one row (33%), 2 rows remaining")
})
