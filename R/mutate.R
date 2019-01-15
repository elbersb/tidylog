#' Wrapper around dplyr::mutate and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{mutate}
#' @param ... see \link[dplyr]{mutate}
#' @return see \link[dplyr]{mutate}
#' @import dplyr
#' @export
mutate <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate, "mutate", ...)
}

#' @rdname mutate
#' @export
mutate_all <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_all, "mutate_all", ...)
}

#' @rdname mutate
#' @export
mutate_if <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_if, "mutate_if", ...)
}

#' @rdname mutate
#' @export
mutate_at <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_at, "mutate_at", ...)
}

log_mutate <- function(.data, fun, funname, ...) {
    cols <- names(.data)
    newdata <- fun(.data, ...)
    for (var in names(newdata)) {
        if (var %in% cols) {
            # existing var
            compare <- newdata[[var]] != .data[[var]]
            if (any(compare)) {
                n <- sum(compare)
                p <- round(100 * (n / length(compare)))
                cat(glue::glue("{funname}: changed {plural(n, 'value')} ({p}%) of '{var}'"), "\n")
            }
        } else {
            # new var
            n <- length(unique(newdata[[var]]))
            cat(glue::glue("{funname}: new variable '{var}' with {plural(n, 'value', 'unique ')}"),
                "\n")
        }
    }
    newdata
}
