plural <- function(n_items, noun, mid = "") {
    if (n_items == 1) {
        paste0("one ", mid, noun)
    } else {
        paste0(format(n_items, big.mark = ",", scientific = FALSE), " ", mid, noun, "s")
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

#' @import clisymbols
format_list <- function(items) {
    if (length(items) <= 5) {
        paste0(items, collapse = ", ")
    } else {
        paste0(c(items[1:5], clisymbols::symbol$ellipsis), collapse = ", ")
    }
}

get_type <- function(v) {
    if (is.ordered(v)) {
        "ordered factor"
    } else if (is.factor(v)) {
        "factor"
    } else if (inherits(v, "Date")) {
        "Date"
    } else if (inherits(v, "units")) {
        "units"
    } else {
        typeof(v)
    }
}

get_groups <- function(.data) {
    if (!is.null(attr(.data, "groups"))) {
        # support for dplyr >= 0.8
        groups <- attr(.data, "groups")
        return(utils::head(names(groups), -1))
    } else {
        # support for dplyr < 0.8
        return(attr(.data, "vars"))
    }
}

display <- function(text) {
    functions <- getOption("tidylog.display")
    if (is.null(functions)) {
        message(text)
    } else if (is.list(functions)) {
        for (f in functions) {
            if (is.function(f)) {
                f(text)
            } else {
                warning("tidylog.display needs to be set to either NULL or a list of functions")
            }
        }
    } else {
        warning("tidylog.display needs to be set to either NULL or a list of functions")
    }
}

should_display <- function() {
    is.null(getOption("tidylog.display")) | length(getOption("tidylog.display")) > 0
}

#' outputs some information about the data frame/tbl
#'
#' @param .data a tbl/data frame
#' @return same as .data
#' @examples
#' tidylog(mtcars)
#' #> tidylog: data.frame with 32 rows and 11 columns
#' @export
tidylog <- function(.data) {
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(.data)
    }

    if ("grouped_df" %in% class(.data)) {
        type <- glue::glue("grouped tibble")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else {
        type <- "data.frame"
    }

    display(glue::glue("tidylog: {type} with {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}"))
    .data
}
