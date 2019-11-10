#' Wrapper around dplyr::filter and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @return see \link[dplyr]{filter}
#' @examples
#' filter(mtcars, mpg > 20)
#' #> filter: removed 18 rows (56%), 14 remaining
#' filter(mtcars, mpg > 100)
#' #> filter: removed all rows (100%)
#' @import dplyr
#' @import tidyr
#' @export
filter <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter, .funname = "filter", ...)
}

#' @rdname filter
#' @export
filter_all <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter_all, .funname = "filter_all", ...)
}

#' @rdname filter
#' @export
filter_if <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter_if, .funname = "filter_if", ...)
}

#' @rdname filter
#' @export
filter_at <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::filter_at, .funname = "filter_at", ...)
}

#' @rdname filter
#' @export
distinct <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct, .funname = "distinct", ...)
}

#' @rdname filter
#' @export
distinct_all <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct_all, .funname = "distinct_all", ...)
}

#' @rdname filter
#' @export
distinct_if <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct_if, .funname = "distinct_if", ...)
}

#' @rdname filter
#' @export
distinct_at <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::distinct_at, .funname = "distinct_at", ...)
}

#' @rdname filter
#' @export
top_n <- function(.data, ...) {
    log_filter(.data, .fun = dplyr::top_n, .funname = "top_n", ...)
}

#' @rdname filter
#' @export
drop_na <- function(.data, ...) {
    log_filter(.data, .fun = tidyr::drop_na, .funname = "drop_na", ...)
}


log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    group_status <- ifelse(dplyr::is.grouped_df(newdata), " (grouped)", "")

    n <- nrow(.data) - nrow(newdata)
    if (n == 0) {
        display(glue::glue("{.funname}{group_status}: no rows removed"))
    } else if (n == nrow(.data)) {
        display(glue::glue("{.funname}{group_status}: removed all rows (100%)"))
    } else {
        total <- nrow(.data)
        display(glue::glue("{.funname}{group_status}: ",
            "removed {plural(n, 'row')} ",
            "({percent(n, {total})}), {plural(nrow(newdata), 'row')} remaining"))
    }
    newdata
}
