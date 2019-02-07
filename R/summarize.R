#' Wrapper around dplyr::summarize and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{summarize}
#' @param ... see \link[dplyr]{summarize}
#' @return see \link[dplyr]{summarize}
#' @import dplyr
#' @export
summarize <- function(.data, ...){
    log_summarize(.data, dplyr::summarize, "summarize", ...)
}

#' @rdname summarize
#' @export
summarize_all <- function(.data, ...){
    log_summarize(.data, dplyr::summarize_all, "summarize_all", ...)
}

#' @rdname summarize
#' @export
summarize_at <- function(.data, ...){
    log_summarize(.data, dplyr::summarize_at, "summarize_at", ...)
}

#' @rdname summarize
#' @export
summarize_each <- function(.data, ...){
    log_summarize(.data, dplyr::summarize_each, "summarize_each", ...)
}

#' @rdname summarize
#' @export
summarize_if <- function(.data, ...){
    log_summarize(.data, dplyr::summarize_if, "summarize_if", ...)
}

#' Wrapper around dplyr::summarise and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{summarise}
#' @param ... see \link[dplyr]{summarise}
#' @return see \link[dplyr]{summarise}
#' @import dplyr
#' @export
summarise <- function(.data, ...){
    log_summarize(.data, dplyr::summarise, "summarise", ...)
}

#' @rdname summarise
#' @export
summarise_all <- function(.data, ...){
    log_summarize(.data, dplyr::summarise_all, "summarise_all", ...)
}

#' @rdname summarise
#' @export
summarise_at <- function(.data, ...){
    log_summarize(.data, dplyr::summarise_at, "summarise_at", ...)
}

#' @rdname summarise
#' @export
summarise_each <- function(.data, ...){
    log_summarize(.data, dplyr::summarise_each, "summarise_each", ...)
}

#' @rdname summarise
#' @export
summarise_if <- function(.data, ...){
    log_summarize(.data, dplyr::summarise_if, "summarise_if", ...)
}

log_summarize <- function(.data, fun, funname, ...){
    newdata <- fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    group_names <- dplyr::groups(newdata)
    group_length <- length(group_names)
    if (group_length > 0) {
        display(glue::glue(
            "{funname}: {plural(group_length, 'group')} remaining ",
            "({format_list(group_names)})"))
    } else {
        display(glue::glue(
            "{funname}: {plural(group_length, 'group')} remaining ",
            "{format_list(group_names)}"))
    }

    newdata
}
