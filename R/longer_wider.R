#' Wrapper around tidyr's pivot_longer/pivot_wider/spread/gather
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[tidyr]{pivot_longer}
#' @param ... see \link[tidyr]{pivot_longer}
#' @return see \link[tidyr]{pivot_longer}
#' @examples
#' # create id
#' mtcars$id <- seq_len(nrow(mtcars))
#' pivot_longer(mtcars, -id, names_to = "var", values_to = "value")
#' #> pivot_longer: reorganized (mpg, cyl, disp, hp, drat, …)
#' #>               into (var, value) [was 32x12, now 352x3]
#' @import tidyr
#' @export
pivot_longer <- function(.data, ...) {
    log_longer_wider(.data, .fun = tidyr::pivot_longer, .funname = "pivot_longer", ...)
}

#' @rdname pivot_longer
#' @export
pivot_wider <- function(.data, ...) {
    log_longer_wider(.data, .fun = tidyr::pivot_wider, .funname = "pivot_wider", ...)
}

#' @rdname pivot_longer
#' @export
gather <- function(.data, ...) {
    log_longer_wider(.data, .fun = tidyr::gather, .funname = "gather", ...)
}

#' @rdname pivot_longer
#' @export
spread <- function(.data, ...) {
    log_longer_wider(.data, .fun = tidyr::spread, .funname = "spread", ...)
}

log_longer_wider <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    newcols <- setdiff(names(newdata), names(.data))
    oldcols <- setdiff(names(.data), names(newdata))

    display(glue::glue(
        "{.funname}: ",
        "reorganized ({format_list(oldcols)}) ",
        "into ({format_list(newcols)}) ",
        "[was {nrow(.data)}x{ncol(.data)}, ",
        "now {nrow(newdata)}x{ncol(newdata)}]"
    ))

    newdata
}
