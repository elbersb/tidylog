#' Wrapper around dplyr::group_by and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{group_by}
#' @param ... see \link[dplyr]{group_by}
#' @return see \link[dplyr]{group_by}
#' @examples
#' mtcars <- group_by(mtcars, am, cyl)
#' #> group_by: 2 grouping variables (am, cyl)
#' mtcars <- ungroup(mtcars)
#' #> ungroup: no grouping variables left
#' @import dplyr
#' @export
group_by <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by, .funname = "group_by", ...)
}

#' @rdname group_by
#' @export
group_by_all <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by_all, .funname = "group_by_all", ...)
}

#' @rdname group_by
#' @export
group_by_if <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by_if, .funname = "group_by_if", ...)
}

#' @rdname group_by
#' @export
group_by_at <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::group_by_at, .funname = "group_by_at", ...)
}

#' @rdname group_by
#' @export
ungroup <- function(.data, ...) {
    log_group_by(.data, .fun = dplyr::ungroup, .funname = "ungroup", ...)
}

log_group_by <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    group_vars <- get_groups(newdata)
    if (!is.null(group_vars)) {
    display(glue::glue(
        "{.funname}: {plural(length(group_vars), 'grouping variable')} ",
        "({format_list(group_vars)})"))
    } else {
        display(glue::glue("{.funname}: no grouping variables"))
    }
    newdata
}
