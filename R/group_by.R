#' Wrapper around dplyr::group_by and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{group_by}
#' @param ... see \link[dplyr]{group_by}
#' @return see \link[dplyr]{group_by}
#' @import dplyr
#' @export
group_by <- function(.data, ...) {
    log_group_by(.data, dplyr::group_by, "group_by", ...)
}

#' @rdname group_by
#' @export
group_by_all <- function(.data, ...) {
    log_group_by(.data, dplyr::group_by_all, "group_by_all", ...)
}

#' @rdname group_by
#' @export
group_by_if <- function(.data, ...) {
    log_group_by(.data, dplyr::group_by_if, "group_by_if", ...)
}

#' @rdname group_by
#' @export
group_by_at <- function(.data, ...) {
    log_group_by(.data, dplyr::group_by_at, "group_by_at", ...)
}

log_group_by <- function(.data, fun, funname, ...) {
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data)) {
        return(newdata)
    }
    groups <- get_groups(newdata)
    cat(glue::glue(
        "{funname}: {plural(groups[[1]], 'group')} ",
        "({format_list(groups[[2]])})"), "\n")
    newdata
}
