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
