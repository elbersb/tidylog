#' @export
pivot_longer <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::pivot_longer, .funname = "pivot_longer", ...)
}

#' @export
pivot_wider <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::pivot_wider, .funname = "pivot_wider", ...)
}

#' @export
gather <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::gather, .funname = "gather", ...)
}


#' @export
spread <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::spread, .funname = "spread", ...)
}

#' @export
separate_wider_position <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::separate_wider_position, .funname = "separate_wider_position", ...)
}

#' @export
separate_wider_delim <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::separate_wider_delim, .funname = "separate_wider_delim", ...)
}

#' @export
separate_wider_regex <- function(data, ...) {
    log_longer_wider(data, .fun = tidyr::separate_wider_regex, .funname = "separate_wider_regex", ...)
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
