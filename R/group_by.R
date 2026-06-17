# Logging of functions that affect grouping, such as dplyr::group_by.
log_group_by <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }
    group_vars <- get_groups(.newdata)
    if (is.null(group_vars)) {
        display(glue::glue("{.funname}: no grouping variables remain"))
    } else {
        display(glue::glue(
            "{.funname}: {plural(length(group_vars), 'grouping variable')} ",
            "({format_list(group_vars)})"))
    }
}
