#' Wrapper around tidyr::gather
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[tidyr]{gather}
#' @param ... see \link[tidyr]{gather}
#' @return see \link[tidyr]{gather}
#' @examples
#' mtcars %>%
#'   mutate(id = 1:n()) %>%
#'   gather("col", "data", -id)
#' #> gather: was 32 rows and 12 columns; now 352 rows and 3 columns;
#' #> reorganized data from (mpg, cyl, disp, hp, drat, …) into (col, data)
#' @import tidyr
#' @export
gather <- function(.data, ...) {
    log_gather(.data, .fun = tidyr::gather, .funname = "gather", ...)
}

log_gather <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    newcols <- setdiff(names(newdata), names(.data))
    oldcols <- setdiff(names(.data), names(newdata))

    display(glue::glue(
        "{.funname}: was {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}",
        "; now {plural(nrow(newdata), 'row')} and ",
        "{plural(ncol(newdata), 'column')}; ",
        "reorganized data from ({format_list(oldcols)}) into ({format_list(newcols)})"
    ))

    newdata
}
