#' Wrapper around dplyr::mutate and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{mutate}
#' @param ... see \link[dplyr]{mutate}
#' @return see \link[dplyr]{mutate}
#' @examples
#' mutate(mtcars, new_var = 1)
#' #> mutate: new variable 'new_var' with one unique value and 0% NA
#' mutate(mtcars, new_var = NA)
#> #> mutate: new variable 'new_var' with one unique value and 100% NA
#' @import dplyr
#' @import tidyr
#' @export
mutate <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate, .funname = "mutate", ...)
}

#' @rdname mutate
#' @export
mutate_all <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_all, .funname = "mutate_all", ...)
}

#' @rdname mutate
#' @export
mutate_if <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_if, .funname = "mutate_if", ...)
}

#' @rdname mutate
#' @export
mutate_at <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_at, .funname = "mutate_at", ...)
}

#' Wrapper around dplyr::transmute and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr:mutate]{transmute}
#' @param ... see \link[dplyr:mutate]{transmute}
#' @return see \link[dplyr:mutate]{transmute}
#' @examples
#' transmute(mtcars, mpg = mpg * 2)
#' #> transmute: dropped 10 variables (cyl, disp, hp, drat, wt, ...)
#' #> transmute: changed 32 values (100%) of 'mpg' (0 new NA)
#' @import dplyr
#' @export
transmute <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute, .funname = "transmute", ...)
}

#' @rdname transmute
#' @export
transmute_all <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_all, .funname = "transmute_all", ...)
}

#' @rdname transmute
#' @export
transmute_if <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_if, .funname = "transmute_if", ...)
}

#' @rdname transmute
#' @export
transmute_at <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_at, .funname = "transmute_at", ...)
}

#' @rdname mutate
#' @export
add_tally <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::add_tally, .funname = "add_tally", ...)
}

#' @rdname mutate
#' @export
add_count <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::add_count, .funname = "add_count", ...)
}

#' @rdname mutate
#' @export
replace_na <- function(.data, ...) {
    log_mutate(.data, .fun = tidyr::replace_na, .funname = "replace_na", ...)
}

#' @rdname mutate
#' @export
fill <- function(.data, ...) {
    log_mutate(.data, .fun = tidyr::fill, .funname = "fill", ...)
}

log_mutate <- function(.data, .fun, .funname, ...) {
    cols <- names(.data)
    newdata <- .fun(.data, ...)

    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    group_status <- ifelse(dplyr::is.grouped_df(newdata), " (grouped)", "")

    if (grepl("transmute", .funname)) {
        dropped_vars <- setdiff(names(.data), names(newdata))
        n <- length(dropped_vars)
        if (ncol(newdata) == 0) {
            display(glue::glue("{.funname}{group_status}: dropped all variables"))
            return(newdata)
        } else if (length(dropped_vars) > 0) {
            display(glue::glue("{.funname}{group_status}: dropped {plural(n, 'variable')}",
                           " ({format_list(dropped_vars)})"))
        }
    }

    has_changed <- FALSE
    for (var in names(newdata)) {
        # new var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            display(glue::glue("{.funname}{group_status}: new variable '{var}' ",
                "with {plural(n, 'value', 'unique ')} and {p_na} NA"))
        } else {
            # existing var
            # use identical to account for missing values - this is fast
            if (identical(newdata[[var]], .data[[var]])) {
                next
            }
            has_changed <- TRUE
            old <- .data[[var]]
            new <- newdata[[var]]
            typeold <- ifelse(is.factor(old), "factor", typeof(old))
            typenew <- ifelse(is.factor(new), "factor", typeof(new))

            if (typeold == "factor" & typenew == "factor") {
                # when factor, compare based on character values
                # this will include both changes in the factor levels and recodes
                old <- as.character(old)
                new <- as.character(new)
            }

            if (typeold == typenew) {
                # same type
                different <- new != old
                different[is.na(new) & !is.na(old)] <- TRUE
                different[!is.na(new) & is.na(old)] <- TRUE
                different[is.na(new) & is.na(old)] <- FALSE
                n <- sum(different)
                p <- percent(n, length(different))
                new_na <- sum(is.na(new)) - sum(is.na(old))
                na_text <- glue::glue("{abs(new_na)} ",
                                      ifelse(new_na >= 0, "new", "fewer"), " NA")
                display(glue::glue("{.funname}{group_status}: changed {plural(n, 'value')} ",
                    "({p}) of '{var}' ({na_text})"))
            } else {
                # different type
                new_na <- sum(is.na(new)) - sum(is.na(old))
                if (new_na == length(new)) {
                    display(glue::glue("{.funname}{group_status}: converted '{var}' from {typeold}",
                        " to {typenew} (now 100% NA)"))
                } else {
                    na_text <- glue::glue("{abs(new_na)} ",
                                          ifelse(new_na >= 0, "new", "fewer"), " NA")
                    display(glue::glue("{.funname}{group_status}: converted '{var}' from {typeold}",
                        " to {typenew} ({na_text})"))
                }
            }
        }
    }

    if (!has_changed) {
        display(glue::glue("{.funname}{group_status}: no changes"))
    }
    newdata
}
