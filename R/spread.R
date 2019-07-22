#' Wrapper around tidyr::spread
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[tidyr]{spread}
#' @param ... see \link[tidyr]{spread}
#' @return see \link[tidyr]{spread}
#' @examples
#' mtcars %>%
#'   mutate(id = 1:n()) %>%
#'   gather("col", "data", -id) %>%
#'   spread(col, data)
#' #> spread: was 352 rows and 3 columns, now 32 rows and 12 columns;
#' #> reorganized data from (col, data) into (am, carb, cyl, disp, drat, …)
#' @import tidyr
#' @export
spread <- function(.data, ...) {
    log_spread(.data, .fun = tidyr::spread, .funname = "spread", ...)
}

log_spread <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    newcols <- setdiff(names(newdata), names(.data))
    oldcols <- setdiff(names(.data), names(newdata))

    display(glue::glue(
        "{.funname}: was {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}, ",
        "now {plural(nrow(newdata), 'row')} and ",
        "{plural(ncol(newdata), 'column')}; ",
        "reorganized data from ({format_list(oldcols)}) into ({format_list(newcols)})"
    ))

    newdata
}
