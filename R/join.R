#' Wrapper around dplyr::inner_join and related functions
#' that prints information about the operation
#'
#'
#' @param x a tbl; see \link[dplyr:join]{inner_join}
#' @param ... see \link[dplyr:join]{inner_join}
#' @return see \link[dplyr:join]{inner_join}
#' @examples
#' left_join(dplyr::band_members, dplyr::band_instruments, by = "name")
#' #> left_join: added 0 rows and added one column (plays)
#' full_join(dplyr::band_members, dplyr::band_instruments, by = "name")
#' #> full_join: added one row and added one column (plays)
#' @import dplyr
#' @export
inner_join <- function(x, ...) {
    log_join(x, .fun = dplyr::inner_join, .funname = "inner_join", ...)
}

#' @rdname inner_join
#' @export
full_join <- function(x, ...) {
    log_join(x, .fun = dplyr::full_join, .funname = "full_join", ...)
}

#' @rdname inner_join
#' @export
left_join <- function(x, ...) {
    log_join(x, .fun = dplyr::left_join, .funname = "left_join", ...)
}

#' @rdname inner_join
#' @export
right_join <- function(x, ...) {
    log_join(x, .fun = dplyr::right_join, .funname = "right_join", ...)
}

#' @rdname inner_join
#' @export
anti_join <- function(x, ...) {
    log_join(x, .fun = dplyr::anti_join, .funname = "anti_join", ...)
}

#' @rdname inner_join
#' @export
semi_join <- function(x, ...) {
    log_join(x, .fun = dplyr::semi_join, .funname = "semi_join", ...)
}

log_join <- function(x, .fun, .funname, ...) {
    newdata <- .fun(x, ...)
    if (!"data.frame" %in% class(x) | !should_display()) {
        return(newdata)
    }
    n_rows <- nrow(newdata) - nrow(x)
    text_rows <- ifelse(n_rows >= 0, "added", "removed")
    n_rows <- abs(n_rows)

    cols <- setdiff(names(newdata), names(x))

    if (length(cols) == 0) {
        display(glue::glue("{.funname}: {text_rows} {plural(n_rows, 'row')} and ",
                "added no new columns"))
    } else {
        display(glue::glue("{.funname}: {text_rows} {plural(n_rows, 'row')} and ",
                "added {plural(length(cols), 'column')} ({format_list(cols)})"))
    }

    newdata
}
