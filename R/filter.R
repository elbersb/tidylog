#' @import dplyr
#' @import tidyr
#' @export
filter <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter, .funname = "filter", ...)
}

#' @export
filter_all <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_all, .funname = "filter_all", ...)
}

#' @export
filter_if <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_if, .funname = "filter_if", ...)
}

#' @export
filter_at <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::filter_at, .funname = "filter_at", ...)
}

#' @export
distinct <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct, .funname = "distinct", ...)
}

#' @export
distinct_all <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_all, .funname = "distinct_all", ...)
}

#' @export
distinct_if <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_if, .funname = "distinct_if", ...)
}

#' @export
distinct_at <- function(.tbl, ...) {
    log_filter(.tbl, .fun = dplyr::distinct_at, .funname = "distinct_at", ...)
}

#' @export
top_n <- function(x, ...) {
    log_filter(x, .fun = dplyr::top_n, .funname = "top_n", ...)
}

#' @export
top_frac <- function(x, ...) {
    log_filter(x, .fun = dplyr::top_frac, .funname = "top_frac", ...)
}

#' @export
sample_n <- function(tbl, ...) {
    log_filter(tbl, .fun = dplyr::sample_n, .funname = "sample_n", ...)
}

#' @export
sample_frac <- function(tbl, ...) {
    log_filter(tbl, .fun = dplyr::sample_frac, .funname = "sample_frac", ...)
}

#' @export
slice <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice, .funname = "slice", ...)
}

#' @export
slice_head <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_head, .funname = "slice_head", ...)
}

#' @export
slice_tail <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_tail, .funname = "slice_tail", ...)
}

#' @export
slice_min <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_min, .funname = "slice_min", ...)
}

#' @export
slice_max <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_max, .funname = "slice_max", ...)
}

#' @export
slice_sample <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::slice_sample, .funname = "slice_sample", ...)
}

#' @export
drop_na <- function(data, ...) {
    log_filter(data, .fun = tidyr::drop_na, .funname = "drop_na", ...)
}

display_slice_ties <- function(.olddata, .newdata, .funname, ...) {
    # We must use enquos to account for the NSE of variable names.
    dots <- rlang::enquos(...)

    # The default is `with_ties=TRUE`, so we only check for explicit FALSE.
    if(isFALSE(dots$with_ties)) return()

    # Use explicit grouping when evaluating by= grouping.
    if(!is.null(dots$by)) {
        .olddata <- dplyr::group_by(.olddata, dplyr::across(!!dots$by))
        .newdata <- dplyr::group_by(.newdata, dplyr::across(!!dots$by))
    }

    # slice_min/max don't change number of groups.
    n_groups <- dplyr::n_groups(.newdata)
    old_group_sizes <- dplyr::group_size(.olddata)
    new_group_sizes <- dplyr::group_size(.newdata)

    # Determine the expected final n based on group sizes and arguments provided
    n <- rlang::eval_tidy(dots$n)
    prop <- rlang::eval_tidy(dots$prop)
    # When neither n or prop are provided both are
    if(is.null(n) && is.null(prop)) {
        expected_n <- rep(1, n_groups)
    } else if(!is.null(prop)) {
        expected_n <- as.integer(old_group_sizes * prop) |>
            pmin(old_group_sizes)  # Ensure no more than whole group.
    } else {
        expected_n <- rep(n, n_groups)
    }

    group_size_diff <- new_group_sizes - expected_n
    total_diff <- sum(group_size_diff)
    num_groups_with_ties <- sum(group_size_diff > 0)
    if (num_groups_with_ties == 0) {
        return()
    }

    group_suffix <- if(num_groups_with_ties == 1) {
        ""
    } else {
        glue::glue(" (across {num_groups})",
                   num_groups=plural(num_groups_with_ties, "group"))
    }
    funname_prefix <- format_funname_prefix(.funname, .newdata)
    ws_pre <- paste0(rep(" ", nchar(funname_prefix)), collapse = "")
    display(glue::glue(
        "{ws_pre}{total_diff} rows are ties{group_suffix}"
    ))
}

log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    display_changed_rows(.data, newdata, .funname)

    if(.funname %in% c("slice_min", "slice_max")) {
        display_slice_ties(.data, newdata, .funname, ...)
    }

    newdata
}
