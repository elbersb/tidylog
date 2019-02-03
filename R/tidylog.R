plural <- function(n_items, noun, mid = "") {
    if (n_items == 1) {
        paste0("one ", mid, noun)
    } else {
        paste0(n_items, " ", mid, noun, "s")
    }
}

percent <- function(n, total) {
    p <- round(n / total * 100)
    if (n == total) {
        "100%"
    } else if (p == 100) {
        ">99%"
    } else if (n == 0) {
        "0%"
    } else if (p == 0) {
        "<1%"
    } else {
        paste0(p, "%")
    }
}

format_list <- function(items) {
    if (length(items) <= 5) {
        paste0(items, collapse = ", ")
    } else {
        paste0(c(items[1:5], "\u2026"), collapse = ", ")
    }
}

get_groups <- function(.data) {
    if (!is.null(attr(.data, "groups"))) {
        # support for dplyr >= 0.8
        groups <- attr(.data, "groups")
        list(nrow(groups), head(names(groups), -1))
    } else {
        # support for dplyr < 0.8
        list(length(attr(.data, "group_sizes")),
            attr(.data, "vars"))
    }
}

display <- function(text) {
    message(text)
}

#' outputs some information about the data frame/tbl
#'
#' @param .data a tbl/data frame
#' @return same as .data
#' @export
tidylog <- function(.data) {
    if ("grouped_df" %in% class(.data)) {
        groups <- get_groups(.data)
        type <- glue::glue("grouped tibble ({groups[[1]]} groups: {format_list(groups[[2]])})")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else if ("data.frame" %in% class(.data)) {
        type <- "data.frame"
    } else {
        return(.data)
    }

    display(glue::glue("tidylog: {type} with {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}"))
    .data
}
