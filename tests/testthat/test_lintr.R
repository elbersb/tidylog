if (requireNamespace("lintr", quietly = TRUE)) {
    context("lintr")

    test_that("Package Style", {

        lintr::expect_lint_free(linters = lintr::with_defaults(
            object_usage_linter = NULL,
            object_name_linter = NULL,
            line_length_linter = lintr::line_length_linter(100)))
    })
}
