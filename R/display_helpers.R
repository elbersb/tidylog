plural <- function(n_items, noun, mid = "") {
    if (n_items == 1) {
        paste0("one ", mid, noun)
    } else {
        paste0(format(n_items, big.mark = ",", scientific = FALSE), " ", mid, noun, "s")
    }
}

shorten <- function(str) {
    if (nchar(str) > 25) {
        paste0(substr(str, 1, 23), "..")
    } else {
        str
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
        paste0(c(items[1:5], cli::symbol$ellipsis), collapse = ", ")
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

#' @import rlang
display <- function(text) {
    functions <- getOption("tidylog.display")
    if (is.null(functions)) {
        rlang::inform(text)
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
