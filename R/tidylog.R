plural <- function(n_items, noun, mid = "") {
    if (n_items == 1) {
        paste0("one ", mid, noun)
    } else {
        paste0(n_items, " ", mid, noun, "s")
    }
}

format_list <- function(items) {
    if (length(items) <= 5) {
        paste0(items, collapse = ", ")
    } else {
        paste0(c(items[1:5], "\u2026"), collapse = ", ")
    }
}

#' outputs some information about the data frame/tbl
#'
#' @param .data a tbl/data frame
#' @return same as .data
#' @export
tidylog <- function(.data) {
    if ("grouped_df" %in% class(.data)) {
        type <- paste0("grouped tibble [", length(attr(.data, "group_sizes")), "]")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else if ("data.frame" %in% class(.data)) {
        type <- "data.frame"
    } else {
        return(.data)
    }

    cat(glue::glue("tidylog: {type} with {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}"), "\n")
    .data
}
