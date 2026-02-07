
log_join <- function(x, y, by = NULL, .fun, .funname, .name_x, .name_y, ...) {
    newdata <- .fun(x, y, by = by, ...)
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

    # figure out matched in rows.
    if (!"dplyr_join_by" %in% class(by)) {
        keys <- suppressMessages(dplyr::common_by(by = by, x = x, y = y))
    } else if(all(by$condition == "==")) {
        keys <- by
    } else {
        # If using `join_by()` with more complex logic than `==`, only report
        # the change in row number from the input `x`.
        display_changed_rows(x, newdata, .funname)
        return(newdata)
    }
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
    # data set names
    .name_x <- ifelse(.name_x == ".", "x", shorten(.name_x))
    .name_y <- ifelse(.name_y == ".", "y", shorten(.name_y))
    names_length <- max(nchar(.name_x), nchar(.name_y))
    .name_x <- format(.name_x, justify = "left", width = names_length)
    .name_y <- format(.name_y, justify = "left", width = names_length)
    # white space
    ws_pre <- paste0(rep(" ", nchar(.funname)), collapse = "")
    ws_post <- paste0(rep(" ", names_length), collapse = "")

    if (.funname %in% c("right_join", "inner_join", "semi_join")) {
        display(glue::glue("{ws_pre}  > rows only in {.name_x} ({stats_str$only_in_x})"))
    } else {
        display(glue::glue("{ws_pre}  > rows only in {.name_x}  {stats_str$only_in_x}"))
    }
    if (.funname %in% c("left_join", "inner_join", "semi_join", "anti_join")) {
        display(glue::glue("{ws_pre}  > rows only in {.name_y} ({stats_str$only_in_y})"))
    } else {
        display(glue::glue("{ws_pre}  > rows only in {.name_y}  {stats_str$only_in_y}"))
    }
    if (.funname == "anti_join") {
        display(glue::glue("{ws_pre}  > matched rows{ws_post}  ({stats_str$matched})"))
    } else {
        display(glue::glue("{ws_pre}  > matched rows{ws_post}   {stats_str$matched}{duplicates}"))
    }
    display(glue::glue("{ws_pre}  >{ws_post}               ={paste0(rep('=', max_n), collapse = '')}="))
    display(glue::glue("{ws_pre}  > rows total{ws_post}     {stats_str$total}"))

    newdata
}
