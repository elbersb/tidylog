#' Wrapper around dplyr::select and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{select}
#' @param ... see \link[dplyr]{select}
#' @return see \link[dplyr]{select}
#' @import dplyr
#' @export
select <- function(.data, ...) {
    log_select(.data, dplyr::select, "select", ...)
}

#' @rdname select
#' @export
select_all <- function(.data, ...) {
    log_select(.data, dplyr::select_all, "select_all", ...)
}

#' @rdname select
#' @export
select_if <- function(.data, ...) {
    log_select(.data, dplyr::select_if, "select_if", ...)
}

#' @rdname select
#' @export
select_at <- function(.data, ...) {
    log_select(.data, dplyr::select_at, "select_at", ...)
}

log_select <- function(.data, fun, funname, ...) {
    cols <- names(.data)
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    dropped_vars <- setdiff(cols, names(newdata))
    n <- length(dropped_vars)
    if (ncol(newdata) == 0) {
        display(glue::glue("{funname}: dropped all variables"))
    } else if (length(dropped_vars) > 0) {
        display(glue::glue("{funname}: dropped {plural(n, 'variable')}",
                       " ({format_list(dropped_vars)})"))
    }
    newdata
}
