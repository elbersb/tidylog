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
    if (n == 0) {
        cat(glue::glue("filter: no rows removed"), "\n")
    } else if (n == nrow(.data)) {
        cat(glue::glue("filter: removed all rows (100%)"), "\n")
    } else {
        cat(glue::glue("filter: removed {plural(n, 'row')} ({percent(n, nrow(.data))})"), "\n")
    }
    newdata
}
