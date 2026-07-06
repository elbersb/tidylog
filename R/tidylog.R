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

#' Format a list of items as a comma-separated string
#'
#' @description
#' Renders `items` as a comma-separated string, truncating to the first 5
#' items with a trailing ellipsis if there are more.
#'
#' @param items Character vector of items to render.
#' @param markers Optional character vector the same length as `items`,
#'   used to suffix individual items. Use empty "" if no marker. May be a
#'   factor if sort order should differ from plain character sort
#'   -- `sort()` honors factor levels.
#'
#' @details
#' If a marked item is swallowed by truncation, the ellipsis is suffixed
#' with the distinct marker(s) found among hidden items, sorted and
#' concatenated, so a marker is never silently dropped.
#'
#' @return A single string.
format_list <- function(items, markers = NULL) {
    if (!is.null(markers) && length(markers) != length(items)) {
        stop("`markers` must be the same length as `items`")
    }

    add_marker <- function(x, m) paste0(x, m)

    num_start <- min(5, length(items))
    start <- items[seq_len(num_start)]
    decorated_start <- if (is.null(markers)) {
        start
    } else {
        add_marker(start, markers[seq_len(num_start)])
    }

    if (length(items) <= 5) {
        return(paste0(decorated_start, collapse = ", "))
    }

    hidden_markers <- if (!is.null(markers)) {
        sort(unique(markers[-seq_len(num_start)]))
    }

    truncation_symbol <- if (length(hidden_markers) > 0) {
        add_marker(cli::symbol$ellipsis, paste0(as.character(hidden_markers), collapse = ""))
    } else {
        cli::symbol$ellipsis
    }

    paste0(c(decorated_start, truncation_symbol), collapse = ", ")
}

# Returns whitespace of the same width as x
replace_with_ws <- function(x) {
    paste0(rep(" ", nchar(x, type = "width")), collapse = "")
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
