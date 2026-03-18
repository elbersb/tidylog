# Logger for functions that summarize data, such as dplyr::summarize.
log_summarize <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }

    group_vars <- get_groups(.newdata)
    group_length <- length(group_vars)
    if (group_length > 0) {
        display(glue::glue(
            "{.funname}: now {plural(nrow(.newdata), 'row')} and ",
            "{plural(ncol(.newdata), 'column')}, ",
            "{plural(group_length, 'group variable')} remaining ",
            "({format_list(group_vars)})"))
    } else {
        display(glue::glue(
            "{.funname}: now {plural(nrow(.newdata), 'row')} and ",
            "{plural(ncol(.newdata), 'column')}, ungrouped"))
    }
}
