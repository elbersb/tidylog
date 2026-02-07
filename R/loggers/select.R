log_select <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    dropped_vars <- setdiff(cols, names(newdata))
    renamed_vars <- setdiff(names(newdata), cols)

    if (ncol(newdata) == 0) {
        display(glue::glue("{.funname}: dropped all variables"))
    } else if (length(renamed_vars) > 0 & length(renamed_vars) == length(dropped_vars)) {
        # renamed only
        display(glue::glue("{.funname}: renamed {plural(length(renamed_vars), 'variable')}",
                           " ({format_list(renamed_vars)})"))
    } else if (length(dropped_vars) > 0 & length(renamed_vars) > 0) {
        # dropped & renamed
        n_dropped <- length(dropped_vars) - length(renamed_vars)
        display(glue::glue("{.funname}: ",
                           "renamed {plural(length(renamed_vars), 'variable')}",
                           " ({format_list(renamed_vars)})",
                           " and dropped {plural(n_dropped, 'variable')}"))
    } else if (length(dropped_vars) > 0) {
        # dropped only
        display(glue::glue("{.funname}: dropped {plural(length(dropped_vars), 'variable')}",
                           " ({format_list(dropped_vars)})"))
    } else {
        # no dropped, no removed
        if (all(names(newdata) == cols)) {
            display(glue::glue("{.funname}: no changes"))
        } else {
            display(glue::glue("{.funname}: columns reordered",
                               " ({format_list(names(newdata))})"))
        }
    }

    newdata
}
