if (requireNamespace("lintr", quietly = TRUE)) {
    context("lintr")

    test_that("Package Style", {
        lintr::expect_lint_free()
    })
}
