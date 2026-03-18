# Logging of functions that change number of rows, such as dplyr::filter.

display_slice_ties <- function(.olddata, .newdata, .funname, ...) {
    # We must use enquos to account for the NSE of variable names.
    dots <- rlang::enquos(...)

    # If with_ties is explicitly FALSE, don't add any logging.
    # (The unspecified default is `with_ties=TRUE`.)
    if(isFALSE(dots$with_ties)) return()

    # Use explicit grouping when evaluating `by=` grouping.
    if(!is.null(dots$by)) {
        .olddata <- dplyr::group_by(.olddata, dplyr::across(!!dots$by))
    }

    # slice_min/max don't change number of groups from olddata to newdata.
    n_groups <- dplyr::n_groups(.olddata)
    old_group_sizes <- dplyr::group_size(.olddata)

    # Determine the expected final n based on group sizes and arguments provided
    n <- rlang::eval_tidy(dots$n)
    prop <- rlang::eval_tidy(dots$prop)
    # When neither n or prop are provided both are
    if(is.null(n) && is.null(prop)) {
        expected_n <- n_groups
    } else if(!is.null(prop)) {
        expected_n <- as.integer(old_group_sizes * prop) |>
            pmin(old_group_sizes) |>   # Ensure no more than whole group.
            sum()
    } else {
        expected_n <- n * n_groups
    }

    total_diff <- nrow(.newdata) - expected_n

    # Only display something if ties are found.
    if (total_diff > 0)
    {
        funname_prefix <- format_funname_prefix(.funname, .newdata)
        ws_pre <- paste0(rep(" ", nchar(funname_prefix)), collapse = "")

        display(glue::glue(
            "{ws_pre}with_ties: {total_diff} rows are ties"
        ))
    }

}

log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    display_changed_rows(.data, newdata, .funname)

    if(.funname %in% c("slice_min", "slice_max")) {
        display_slice_ties(.data, newdata, .funname, ...)
    }

    newdata
}
