#' Wrapper around dplyr::rename and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{rename}
#' @param ... see \link[dplyr]{rename}
#' @return see \link[dplyr]{rename}
#' @examples
#' rename(mtcars, MPG = mpg, WT = wt)
#' #> rename: renamed 2 variables (MPG, WT)
#' @import dplyr
#' @export
rename <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename, .funname = "rename", ...)
}

#' @rdname rename
#' @export
rename_all <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename_all, .funname = "rename_all", ...)
}

#' @rdname rename
#' @export
rename_if <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename_if, .funname = "rename_if", ...)
}

#' @rdname rename
#' @export
rename_at <- function(.data, ...) {
    log_rename(.data, .fun = dplyr::rename_at, .funname = "rename_at", ...)
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
