# Logger for functions that change column names, such as dplyr::rename.
log_rename <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    renamed_cols <- setdiff(names(newdata), cols)
    n <- length(renamed_cols)
    if (n > 0) {
        display(glue::glue("{.funname}: renamed {plural(n, 'variable')}",
                       " ({format_list(renamed_cols)})"))
    }
    newdata
}
