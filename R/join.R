#' Wrapper around dplyr::inner_join and related functions
#' that prints information about the operation
#'
#'
#' @param x a tbl; see \link[dplyr:join]{inner_join}
#' @param y a tbl; see \link[dplyr:join]{inner_join}
#' @param by a vector; see \link[dplyr:join]{inner_join}
#' @param ... see \link[dplyr:join]{inner_join}
#' @return see \link[dplyr:join]{inner_join}
#' @examples
#' left_join(dplyr::band_members, dplyr::band_instruments, by = "name")
#' #> left_join: added 0 rows and added one column (plays)
#' full_join(dplyr::band_members, dplyr::band_instruments, by = "name")
#' #> full_join: added one row and added one column (plays)
#' @import dplyr
#' @export
inner_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::inner_join, .funname = "inner_join", ...)
}

#' @rdname inner_join
#' @export
full_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::full_join, .funname = "full_join", ...)
}

#' @rdname inner_join
#' @export
left_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::left_join, .funname = "left_join", ...)
}

#' @rdname inner_join
#' @export
right_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::right_join, .funname = "right_join", ...)
}

#' @rdname inner_join
#' @export
anti_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::anti_join, .funname = "anti_join", ...)
}

#' @rdname inner_join
#' @export
semi_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::semi_join, .funname = "semi_join", ...)
}

log_join <- function(x, y, by, .fun, .funname, ...) {
    newdata <- .fun(x, y, by, ...)
    if (!"data.frame" %in% class(x) | !should_display()) {
        return(newdata)
    }

    # columns
    cols <- setdiff(names(newdata), names(x))
    if (length(cols) == 0) {
        display(glue::glue("{.funname}: added no new columns"))
    } else {
        display(glue::glue("{.funname}: ",
            "added {plural(length(cols), 'column')} ({format_list(cols)})"))
    }

    # figure out matched in rows
    keys <- suppressMessages(dplyr::common_by(by = by, x = x, y = y))
    cols_x <- x[, keys$x]
    cols_y <- y[, keys$y]

    stats <- list(
        only_in_x = nrow(suppressMessages(dplyr::anti_join(cols_x, cols_y,
                                                           by = stats::setNames(keys$y, keys$x)))),
        only_in_y = nrow(suppressMessages(dplyr::anti_join(cols_y, cols_x,
                                                           by = stats::setNames(keys$x, keys$y)))),
        total = nrow(newdata)
    )

    # figure out matched
    if (.funname %in% c("inner_join", "semi_join")) {
        stats$matched <- stats$total
    }
    else if (.funname == "full_join") {
        stats$matched <- stats$total - stats$only_in_x - stats$only_in_y
    }
    else if (.funname == "left_join") {
        stats$matched <- stats$total - stats$only_in_x
    }
    else if (.funname == "right_join") {
        stats$matched <- stats$total - stats$only_in_y
    }
    else if (.funname == "anti_join") {
        stats$matched <- nrow(x) - stats$total
    }

    # format to same width
    stats_str <- lapply(stats, function(x) formatC(x, big.mark = ","))
    max_n <- max(sapply(stats_str, nchar))
    stats_str <- lapply(stats_str, function(x) format(x, justify = "right", width = max_n))
    # white space
    ws <- paste0(rep(" ", nchar(.funname)), collapse = "")

    if (.funname %in% c("right_join", "inner_join", "semi_join")) {
        display(glue::glue("{ws}  rows only in x  ({stats_str$only_in_x})"))
    } else {
        display(glue::glue("{ws}  rows only in x   {stats_str$only_in_x}"))
    }
    if (.funname %in% c("left_join", "inner_join", "semi_join", "anti_join")) {
        display(glue::glue("{ws}  rows only in y  ({stats_str$only_in_y})"))
    } else {
        display(glue::glue("{ws}  rows only in y   {stats_str$only_in_y}"))
    }
    if (.funname == "anti_join") {
        display(glue::glue("{ws}  matched rows    ({stats_str$matched})"))
    } else {
        display(glue::glue("{ws}  matched rows     {stats_str$matched}"))
    }
    display(glue::glue("{ws}                  ={paste0(rep('=', max_n), collapse = '')}="))
    display(glue::glue("{ws}  rows total       {stats_str$total}"))

    newdata
}
