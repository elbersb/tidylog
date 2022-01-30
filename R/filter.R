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

log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    if (dplyr::is.grouped_df(newdata) == TRUE) {
        group_status <- " (grouped)"
        groups_diff <- dplyr::n_groups(.data) - dplyr::n_groups(newdata)
        group_desc <- glue::glue(" (removed {plural(groups_diff, 'group')}, {plural(dplyr::n_groups(newdata), 'group')} remaining)")
    } else {
        group_status <- ""
        group_desc <- ""
    }

    n <- nrow(.data) - nrow(newdata)
    if (n == 0) {
        display(glue::glue("{.funname}{group_status}: no rows removed"))
    } else if (n == nrow(.data)) {
        display(glue::glue("{.funname}{group_status}: removed all rows (100%)"))
    } else {
        total <- nrow(.data)
        display(glue::glue("{.funname}{group_status}: ",
            "removed {plural(n, 'row')} ",
            "({percent(n, {total})}), {plural(nrow(newdata), 'row')} remaining{group_desc}"))
    }
    newdata
}
