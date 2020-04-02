#' @export
inner_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::inner_join, .funname = "inner_join", ...)
}

#' @export
full_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::full_join, .funname = "full_join", ...)
}

#' @export
left_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::left_join, .funname = "left_join", ...)
}

#' @export
right_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::right_join, .funname = "right_join", ...)
}

#' @export
anti_join <- function(x, y, by = NULL, ...) {
    log_join(x, y, by, .fun = dplyr::anti_join, .funname = "anti_join", ...)
}

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
        display(glue::glue("{.funname}: added no columns"))
    } else {
        display(glue::glue("{.funname}: ",
            "added {plural(length(cols), 'column')} ({format_list(cols)})"))
    }

    # figure out matched in rows
    keys <- suppressMessages(dplyr::common_by(by = by, x = x, y = y))
    cols_x <- x[, keys$x, drop = FALSE]
    cols_y <- y[, keys$y, drop = FALSE]

    only_in_x <- suppressMessages(dplyr::anti_join(cols_x, cols_y,
                                                  by = stats::setNames(keys$y, keys$x)))
    only_in_y <- suppressMessages(dplyr::anti_join(cols_y, cols_x,
                                                  by = stats::setNames(keys$x, keys$y)))

    stats <- list(
        only_in_x = nrow(only_in_x),
        only_in_y = nrow(only_in_y),
        total = nrow(newdata)
    )

    # figure out matched & duplicates
    duplicates <- ""
    if (.funname == "inner_join") {
        stats$matched <- stats$total

        if (stats$matched > (nrow(x) - stats$only_in_x))
            duplicates <- "    (includes duplicates)"
    } else if (.funname == "full_join") {
        stats$matched <- stats$total - stats$only_in_x - stats$only_in_y

        if (stats$matched > (nrow(x) - stats$only_in_x))
            duplicates <- "    (includes duplicates)"
    } else if (.funname == "left_join") {
        stats$matched <- stats$total - stats$only_in_x

        if (stats$matched > (nrow(x) - stats$only_in_x))
            duplicates <- "    (includes duplicates)"
    } else if (.funname == "right_join") {
        stats$matched <- stats$total - stats$only_in_y

        if (stats$matched > (nrow(y) - stats$only_in_y))
            duplicates <- "    (includes duplicates)"
    } else if (.funname == "anti_join") {
        stats$matched <- nrow(x) - stats$total
        # by definition, no duplicates
    } else if (.funname == "semi_join") {
        stats$matched <- stats$total
        # by definition, no duplicates
    }

    # format to same width
    stats_str <- lapply(stats, function(x) formatC(x, big.mark = ","))
    max_n <- max(sapply(stats_str, nchar))
    stats_str <- lapply(stats_str, function(x) format(x, justify = "right", width = max_n))
    # white space
    ws <- paste0(rep(" ", nchar(.funname)), collapse = "")

    if (.funname %in% c("right_join", "inner_join", "semi_join")) {
        display(glue::glue("{ws}  > rows only in x  ({stats_str$only_in_x})"))
    } else {
        display(glue::glue("{ws}  > rows only in x   {stats_str$only_in_x}"))
    }
    if (.funname %in% c("left_join", "inner_join", "semi_join", "anti_join")) {
        display(glue::glue("{ws}  > rows only in y  ({stats_str$only_in_y})"))
    } else {
        display(glue::glue("{ws}  > rows only in y   {stats_str$only_in_y}"))
    }
    if (.funname == "anti_join") {
        display(glue::glue("{ws}  > matched rows    ({stats_str$matched})"))
    } else {
        display(glue::glue("{ws}  > matched rows     {stats_str$matched}{duplicates}"))
    }
    display(glue::glue("{ws}  >                 ={paste0(rep('=', max_n), collapse = '')}="))
    display(glue::glue("{ws}  > rows total       {stats_str$total}"))

    newdata
}
