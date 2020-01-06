#' Wrapper around dplyr::summarize and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{summarize}
#' @param ... see \link[dplyr]{summarize}
#' @return see \link[dplyr]{summarize}
#' @examples
#' summarize_all(mtcars, mean)
#' #> summarize_all: now one row and 11 columns, ungrouped
#' @import dplyr
#' @export
summarize <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize, .funname = "summarize", ...)
}

#' @rdname summarize
#' @export
summarize_all <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize_all, .funname = "summarize_all", ...)
}

#' @rdname summarize
#' @export
summarize_at <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize_at, .funname = "summarize_at", ...)
}

#' @rdname summarize
#' @export
summarize_if <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize_if, .funname = "summarize_if", ...)
}

#' @rdname summarize
#' @export
summarise <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise, .funname = "summarise", ...)
}

#' @rdname summarize
#' @export
summarise_all <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise_all, .funname = "summarise_all", ...)
}

#' @rdname summarize
#' @export
summarise_at <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise_at, .funname = "summarise_at", ...)
}

#' @rdname summarize
#' @export
summarise_if <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise_if, .funname = "summarise_if", ...)
}

#' @rdname summarize
#' @export
tally <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::tally, .funname = "tally", ...)
}

#' @rdname summarize
#' @export
count <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::count, .funname = "count", ...)
}

#' @rdname summarize
#' @export
uncount <- function(.data, ...) {
    log_summarize(.data, .fun = tidyr::uncount, .funname = "uncount", ...)
}

log_summarize <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    group_vars <- get_groups(newdata)
    group_length <- length(group_vars)
    if (group_length > 0) {
        display(glue::glue(
            "{.funname}: now {plural(nrow(newdata), 'row')} and ",
            "{plural(ncol(newdata), 'column')}, ",
            "{plural(group_length, 'group variable')} remaining ",
            "({format_list(group_vars)})"))
    } else {
        display(glue::glue(
            "{.funname}: now {plural(nrow(newdata), 'row')} and ",
            "{plural(ncol(newdata), 'column')}, ungrouped"))
    }

    newdata
}
