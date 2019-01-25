#' Wrapper around dplyr::inner_join and related functions
#' that prints information about the operation
#'
#'
#' @param .data a tbl; see \link[dplyr]{inner_join}
#' @param ... see \link[dplyr]{inner_join}
#' @return see \link[dplyr]{inner_join}
#' @import dplyr
#' @export
inner_join <- function(.data, ...) {
    log_join(.data, dplyr::inner_join, "inner_join", ...)
}

#' @rdname inner_join
#' @export
full_join <- function(.data, ...) {
    log_join(.data, dplyr::full_join, "full_join", ...)
}

#' @rdname inner_join
#' @export
left_join <- function(.data, ...) {
    log_join(.data, dplyr::left_join, "left_join", ...)
}

#' @rdname inner_join
#' @export
right_join <- function(.data, ...) {
    log_join(.data, dplyr::right_join, "right_join", ...)
}

#' @rdname inner_join
#' @export
anti_join <- function(.data, ...) {
    log_join(.data, dplyr::anti_join, "anti_join", ...)
}

#' @rdname inner_join
#' @export
semi_join <- function(.data, ...) {
    log_join(.data, dplyr::semi_join, "semi_join", ...)
}

log_join <- function(.data, fun, funname, ...) {
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data)) {
        return(newdata)
    }
    n_rows <- nrow(newdata) - nrow(.data)
    n_cols <- ncol(newdata) - ncol(.data)
    text_rows <- ifelse(n_rows >= 0, "added", "removed")
    text_cols <- ifelse(n_cols >= 0, "added", "removed")
    n_rows <- abs(n_rows)
    n_cols <- abs(n_cols)

    cat(glue::glue("{funname}: {text_rows} {plural(n_rows, 'row')} and ",
            "{text_cols} {plural(n_cols, 'column')}"), "\n")

    newdata
}
