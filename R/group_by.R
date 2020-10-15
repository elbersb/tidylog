#' @export
group_by <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by, .funname = "group_by", ...)
}

#' @export
group_by_all <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_all, .funname = "group_by_all", ...)
}

#' @export
group_by_if <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_if, .funname = "group_by_if", ...)
}

#' @export
group_by_at <- function(.tbl, ...) {
    log_group_by(.tbl, .fun = dplyr::group_by_at, .funname = "group_by_at", ...)
}

#' @export
ungroup <- function(x, ...) {
    log_group_by(x, .fun = dplyr::ungroup, .funname = "ungroup", ...)
}

log_group_by <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    group_vars <- get_groups(newdata)
    if (is.null(group_vars)) {
        display(glue::glue("{.funname}: no grouping variables remain"))
    } else {
        display(glue::glue(
            "{.funname}: {plural(length(group_vars), 'grouping variable')} ",
            "({format_list(group_vars)})"))
    }
    newdata
}
