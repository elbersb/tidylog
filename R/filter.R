#' Wrapper around dplyr::filter and related functions
#' that prints information about the operation
#'
#'
#' @param .data a tbl; see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @return see \link[dplyr]{filter}
#' @examples
#' filter(mtcars, mpg > 20)
#' #> filter: removed 18 out of 32 rows (56%)
#' filter(mtcars, mpg > 100)
#' #> filter: removed all rows (100%)
#' @import dplyr
#' @export
filter <- function(.data, ...) {
    log_filter(.data, dplyr::filter, "filter", ...)
}

#' @rdname filter
#' @export
filter_all <- function(.data, ...) {
    log_filter(.data, dplyr::filter_all, "filter_all", ...)
}

#' @rdname filter
#' @export
filter_if <- function(.data, ...) {
    log_filter(.data, dplyr::filter_if, "filter_if", ...)
}

#' @rdname filter
#' @export
filter_at <- function(.data, ...) {
    log_filter(.data, dplyr::filter_at, "filter_at", ...)
}

#' @rdname filter
#' @export
distinct <- function(.data, ...) {
    log_filter(.data, dplyr::distinct, "distinct", ...)
}

#' @rdname filter
#' @export
distinct_all <- function(.data, ...) {
    log_filter(.data, dplyr::distinct_all, "distinct_all", ...)
}

#' @rdname filter
#' @export
distinct_if <- function(.data, ...) {
    log_filter(.data, dplyr::distinct_if, "distinct_if", ...)
}

#' @rdname filter
#' @export
distinct_at <- function(.data, ...) {
    log_filter(.data, dplyr::distinct_at, "distinct_at", ...)
}

#' @rdname filter
#' @export
top_n <- function(.data, ...) {
    log_filter(.data, dplyr::top_n, "top_n", ...)
}


log_filter <- function(.data, fun, funname, ...) {
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    group_status <- ifelse(dplyr::is.grouped_df(newdata), " (grouped)", "")

    n <- nrow(.data) - nrow(newdata)
    if (n == 0) {
        display(glue::glue("{funname}{group_status}: no rows removed"))
    } else if (n == nrow(.data)) {
        display(glue::glue("{funname}{group_status}: removed all rows (100%)"))
    } else {
        total <- nrow(.data)
        display(glue::glue("{funname}{group_status}: removed {n} out of {plural(total, 'row')} ",
            "({percent(n, {total})})"))
    }
    newdata
}
