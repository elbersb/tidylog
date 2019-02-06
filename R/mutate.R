#' Wrapper around dplyr::mutate and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{mutate}
#' @param ... see \link[dplyr]{mutate}
#' @return see \link[dplyr]{mutate}
#' @import dplyr
#' @export
mutate <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate, "mutate", ...)
}

#' @rdname mutate
#' @export
mutate_all <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_all, "mutate_all", ...)
}

#' @rdname mutate
#' @export
mutate_if <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_if, "mutate_if", ...)
}

#' @rdname mutate
#' @export
mutate_at <- function(.data, ...) {
    log_mutate(.data, dplyr::mutate_at, "mutate_at", ...)
}

#' Wrapper around dplyr::transmute and related functions
#' that prints information about the operation
#'
#' @param .data a tbl; see \link[dplyr]{transmute}
#' @param ... see \link[dplyr]{transmute}
#' @return see \link[dplyr]{transmute}
#' @import dplyr
#' @export
transmute <- function(.data, ...) {
    log_mutate(.data, dplyr::transmute, "transmute", ...)
}

#' @rdname transmute
#' @export
transmute_all <- function(.data, ...) {
    log_mutate(.data, dplyr::transmute_all, "transmute_all", ...)
}

#' @rdname transmute
#' @export
transmute_if <- function(.data, ...) {
    log_mutate(.data, dplyr::transmute_if, "transmute_if", ...)
}

#' @rdname transmute
#' @export
transmute_at <- function(.data, ...) {
    log_mutate(.data, dplyr::transmute_at, "transmute_at", ...)
}

log_mutate <- function(.data, fun, funname, ...) {
    cols <- names(.data)
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(newdata)
    }

    if (grepl("transmute", funname)) {
        dropped_vars <- setdiff(names(.data), names(newdata))
        n <- length(dropped_vars)
        if (ncol(newdata) == 0) {
            display(glue::glue("{funname}: dropped all variables"))
            return(newdata)
        } else if (length(dropped_vars) > 0) {
            display(glue::glue("{funname}: dropped {plural(n, 'variable')}",
                           " ({format_list(dropped_vars)})"))
        }
    }

    has_changed <- FALSE
    for (var in names(newdata)) {
        # existing var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            display(glue::glue("{funname}: new variable '{var}' ",
                "with {plural(n, 'value', 'unique ')} and {p_na} NA"))
        } else {
            # new var
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
                # stayed factor
                different <- as.numeric(old) != as.numeric(new)
                different[is.na(new) & !is.na(old)] <- TRUE
                different[!is.na(new) & is.na(old)] <- TRUE
                different[is.na(new) & is.na(old)] <- FALSE
                n <- sum(different)
                p <- percent(n, length(different))

                levels_different <- any(levels(old) != levels(new))

                if (n > 0 & levels_different) {
                    display(glue::glue("{funname}: changed {plural(n, 'value')} ",
                        "({p}) of '{var}', factor levels updated"))
                } else if (n > 0 & !levels_different) {
                    display(glue::glue("{funname}: changed {plural(n, 'value')} ",
                        "({p}) of '{var}'"))
                } else if (n == 0 & levels_different) {
                    display(glue::glue("{funname}: factor levels of '{var}' changed"))
                }
            } else if (typeold == typenew) {
                # same type (except factor)
                different <- new != old
                different[is.na(new) & !is.na(old)] <- TRUE
                different[!is.na(new) & is.na(old)] <- TRUE
                different[is.na(new) & is.na(old)] <- FALSE
                n <- sum(different)
                p <- percent(n, length(different))
                new_na <- sum(is.na(new)) - sum(is.na(old))
                display(glue::glue("{funname}: changed {plural(n, 'value')} ",
                    "({p}) of '{var}' ({new_na} new NA)"))
            } else {
                # different type
                new_na <- sum(is.na(new)) - sum(is.na(old))
                if (new_na == length(new)) {
                    display(glue::glue("{funname}: converted '{var}' from {typeold} ",
                        "to {typenew} (now 100% NA)"))
                } else {
                    display(glue::glue("{funname}: converted '{var}' from {typeold} ",
                        "to {typenew} ({new_na} new NA)"))
                }
            }
        }
    }

    if (!has_changed) {
        display(glue::glue("{funname}: no changes"))
    }
    newdata
}
