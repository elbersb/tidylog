#' Wrapper around tidyr::gather
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[tidyr]{gather}
#' @param ... see \link[tidyr]{gather}
#' @return see \link[tidyr]{gather}
#' @examples
#' # create id
#' mtcars$id <- 1:nrow(mtcars)
#' gathered <- gather(mtcars, "col", "data", -id)
#' #> gather: reorganized (mpg, cyl, disp, hp, drat, …) into (col, data) [was 32x12, now 352x3]
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
        "{.funname}: ",
        "reorganized ({format_list(oldcols)}) ",
        "into ({format_list(newcols)}) ",
        "[was {nrow(.data)}x{ncol(.data)}, ",
        "now {nrow(newdata)}x{ncol(newdata)}]"
    ))

    newdata
}

#' Wrapper around tidyr::spread
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[tidyr]{spread}
#' @param ... see \link[tidyr]{spread}
#' @return see \link[tidyr]{spread}
#' @examples
#' # create id
#' mtcars$id <- 1:nrow(mtcars)
#' gathered <- gather(mtcars, "col", "data", -id)
#' #> gather: reorganized (mpg, cyl, disp, hp, drat, …) into (col, data) [was 32x12, now 352x3]
#' spread(gathered, col, data)
#' #> spread: reorganized (col, data) into (am, carb, cyl, disp, drat, …) [was 352x3, now 32x12]
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
        "{.funname}: ",
        "reorganized ({format_list(oldcols)}) ",
        "into ({format_list(newcols)}) ",
        "[was {nrow(.data)}x{ncol(.data)}, ",
        "now {nrow(newdata)}x{ncol(newdata)}]"
    ))

    newdata
}
