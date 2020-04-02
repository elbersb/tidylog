#' @export
mutate <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate, .funname = "mutate", ...)
}

#' @export
mutate_all <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_all, .funname = "mutate_all", ...)
}

#' @export
mutate_if <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_if, .funname = "mutate_if", ...)
}

#' @export
mutate_at <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::mutate_at, .funname = "mutate_at", ...)
}

#' @export
transmute <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute, .funname = "transmute", ...)
}

#' @export
transmute_all <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_all, .funname = "transmute_all", ...)
}

#' @export
transmute_if <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_if, .funname = "transmute_if", ...)
}

#' @export
transmute_at <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::transmute_at, .funname = "transmute_at", ...)
}

#' @export
add_tally <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::add_tally, .funname = "add_tally", ...)
}

#' @export
add_count <- function(.data, ...) {
    log_mutate(.data, .fun = dplyr::add_count, .funname = "add_count", ...)
}

#' @export
replace_na <- function(.data, ...) {
    log_mutate(.data, .fun = tidyr::replace_na, .funname = "replace_na", ...)
}

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

    # add group status
    prefix <- ifelse(dplyr::is.grouped_df(newdata),
        glue::glue("{.funname} (grouped):"),
        glue::glue("{.funname}:"))

    if (grepl("transmute", .funname)) {
        dropped_vars <- setdiff(names(.data), names(newdata))
        n <- length(dropped_vars)
        if (ncol(newdata) == 0) {
            display(glue::glue("{prefix} dropped all variables"))
            return(newdata)
        } else if (length(dropped_vars) > 0) {
            display(glue::glue("{prefix} dropped {plural(n, 'variable')}",
                           " ({format_list(dropped_vars)})"))
            # replace by spaces
            prefix <- paste0(rep(" ", nchar(prefix)), collapse = "")
        }
    }

    has_changed <- FALSE
    for (var in names(newdata)) {
        # new var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            display(glue::glue("{prefix} new variable '{var}' ",
                "with {plural(n, 'value', 'unique ')} and {p_na} NA"))
            # replace by spaces
            prefix <- paste0(rep(" ", nchar(prefix)), collapse = "")
        } else {
            # existing var
            # use identical to account for missing values - this is fast
            if (identical(newdata[[var]], .data[[var]])) {
                next
            }
            has_changed <- TRUE
            old <- .data[[var]]
            new <- newdata[[var]]
            typeold <- get_type(old)
            typenew <- get_type(new)

            if (typeold == "factor" & typenew == "factor") {
                # when factor, compare based on character values
                # this will include both changes in the factor levels and recodes
                old <- as.character(old)
                new <- as.character(new)
            }

            if (typeold == typenew) {
                # same type
                if (typeold == "list") {
                    different <- sapply(seq_len(length(new)),
                                        function(i) !identical(new[[i]], old[[i]]))
                } else {
                    different <- new != old
                    different[is.na(new) & !is.na(old)] <- TRUE
                    different[!is.na(new) & is.na(old)] <- TRUE
                    different[is.na(new) & is.na(old)] <- FALSE
                }
                n <- sum(different)
                p <- percent(n, length(different))
                new_na <- sum(is.na(new)) - sum(is.na(old))
                na_text <- glue::glue("{abs(new_na)} ",
                                      ifelse(new_na >= 0, "new", "fewer"), " NA")
                display(glue::glue("{prefix} changed {plural(n, 'value')} ",
                    "({p}) of '{var}' ({na_text})"))
                # replace by spaces
                prefix <- paste0(rep(" ", nchar(prefix)), collapse = "")
            } else {
                # different type
                new_na <- sum(is.na(new)) - sum(is.na(old))
                if (new_na == length(new)) {
                    display(glue::glue("{prefix} converted '{var}' from {typeold}",
                        " to {typenew} (now 100% NA)"))
                    # replace by spaces
                    prefix <- paste0(rep(" ", nchar(prefix)), collapse = "")
                } else {
                    na_text <- glue::glue("{abs(new_na)} ",
                                          ifelse(new_na >= 0, "new", "fewer"), " NA")
                    display(glue::glue("{prefix} converted '{var}' from {typeold}",
                        " to {typenew} ({na_text})"))
                    # replace by spaces
                    prefix <- paste0(rep(" ", nchar(prefix)), collapse = "")
                }
            }
        }
    }

    if (!has_changed) {
        display(glue::glue("{prefix} no changes"))
    }
    newdata
}
