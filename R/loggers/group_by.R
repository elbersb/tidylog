
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
