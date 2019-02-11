library("dplyr")
library("tidylog")
context("test_group_by")

test_that("join", {

    expect_message({
        out <- tidylog::inner_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::inner_join(band_members, band_instruments, by = "name"))

    expect_message({
        out <- tidylog::full_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::full_join(band_members, band_instruments, by = "name"))

    expect_message({
        out <- tidylog::left_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::left_join(band_members, band_instruments, by = "name"))

    expect_message({
        out <- tidylog::right_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::right_join(band_members, band_instruments, by = "name"))

    expect_message({
        out <- tidylog::anti_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::anti_join(band_members, band_instruments, by = "name"))

    expect_message({
        out <- tidylog::semi_join(band_members, band_instruments, by = "name")
    })
    expect_equal(out, dplyr::semi_join(band_members, band_instruments, by = "name"))

    expect_silent({
        out <- dplyr::inner_join(band_members, band_instruments, by = "name")
        out <- dplyr::full_join(band_members, band_instruments, by = "name")
        out <- dplyr::left_join(band_members, band_instruments, by = "name")
        out <- dplyr::right_join(band_members, band_instruments, by = "name")
        out <- dplyr::anti_join(band_members, band_instruments, by = "name")
        out <- dplyr::semi_join(band_members, band_instruments, by = "name")
    })
})

test_that("join: argument order", {
    expect_message({
        out <- tidylog::inner_join(y = band_instruments, x = band_members)
    })
    expect_equal(out, dplyr::inner_join(y = band_instruments, x = band_members))
    expect_equal(out, dplyr::inner_join(x = band_members, y = band_instruments))

    expect_message({
        out <- tidylog::inner_join(by = "name", x = band_members, y = band_instruments)
    })
    expect_equal(out, dplyr::inner_join(by = "name", x = band_members, y = band_instruments))
})
