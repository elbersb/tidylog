#' @export
summarize <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarize, .funname = "summarize", ...)
}

#' @export
summarize_all <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_all, .funname = "summarize_all", ...)
}

#' @export
summarize_at <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_at, .funname = "summarize_at", ...)
}

#' @export
summarize_if <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarize_if, .funname = "summarize_if", ...)
}

#' @export
summarise <- function(.data, ...) {
    log_summarize(.data, .fun = dplyr::summarise, .funname = "summarise", ...)
}

#' @export
summarise_all <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_all, .funname = "summarise_all", ...)
}

#' @export
summarise_at <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_at, .funname = "summarise_at", ...)
}

#' @export
summarise_if <- function(.tbl, ...) {
    log_summarize(.tbl, .fun = dplyr::summarise_if, .funname = "summarise_if", ...)
}

#' @export
tally <- function(x, ...) {
    log_summarize(x, .fun = dplyr::tally, .funname = "tally", ...)
}

#' @export
count <- function(x, ...) {
    log_summarize(x, .fun = dplyr::count, .funname = "count", ...)
}

#' @export
uncount <- function(data, ...) {
    log_summarize(data, .fun = tidyr::uncount, .funname = "uncount", ...)
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
