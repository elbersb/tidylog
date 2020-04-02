#' @export
rename <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename, .funname = "rename", ...)
}

#' @export
rename_all <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_all, .funname = "rename_all", ...)
}

#' @export
rename_if <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_if, .funname = "rename_if", ...)
}

#' @export
rename_at <- function(.tbl, ...) {
    log_rename(.tbl, .fun = dplyr::rename_at, .funname = "rename_at", ...)
}

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
