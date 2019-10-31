#' Wrapper around dplyr::ungroup that prints information about the operation
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
ungroup <- function(.data, ...) {
    log_ungroup(.data, .fun = dplyr::ungroup, .funname = "ungroup", ...)
}


log_ungroup <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }
    display(glue::glue(
        "{.funname}: no grouping variables left"))
    newdata
}
