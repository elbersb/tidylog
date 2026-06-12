# Logger for functions that change column names, such as dplyr::rename.
log_rename <- function(.olddata, .newdata, .funname, ...) {
    cols <- names(.olddata)
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }
    renamed_cols <- setdiff(names(.newdata), cols)
    n <- length(renamed_cols)
    if (n > 0) {
        display(glue::glue("{.funname}: renamed {plural(n, 'variable')}",
                       " ({format_list(renamed_cols)})"))
    }
}
