#' Wrapper around dplyr::filter and related functions
#' that prints information about the operation
#'
#'
#' @param .data a tbl; see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @return see \link[dplyr]{filter}
#' @import dplyr
#' @export
filter <- function(.data, ...) {
    log_filter(.data, dplyr::filter, ...)
}

#' @rdname filter
#' @export
filter_all <- function(.data, ...) {
    log_filter(.data, dplyr::filter_all, ...)
}

#' @rdname filter
#' @export
filter_if <- function(.data, ...) {
    log_filter(.data, dplyr::filter_if, ...)
}

#' @rdname filter
#' @export
filter_at <- function(.data, ...) {
    log_filter(.data, dplyr::filter_at, ...)
}

log_filter <- function(.data, fun, ...) {
    newdata <- fun(.data, ...)
    n <- nrow(.data) - nrow(newdata)
    p <- round(100 * n / nrow(.data))
    if (n == 0) {
        cat(glue::glue("filter: no rows removed"), "\n")
    } else if (p == 0) {
        cat(glue::glue("filter: removed {plural(n, 'row')} (<1%)"), "\n")
    } else if (n == nrow(.data)) {
        cat(glue::glue("filter: removed all rows (100%)"), "\n")
    } else if (p == 100) {
        cat(glue::glue("filter: removed {plural(n, 'row')} rows (>99%)"), "\n")
    } else {
        cat(glue::glue("filter: removed {plural(n, 'row')} ({p}%)"), "\n")
    }
    newdata
}
