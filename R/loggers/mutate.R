
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

    dropped_vars <- setdiff(cols, names(newdata))
    if (length(dropped_vars) > 0) {
        # dropped only
        display(glue::glue("{.funname}: dropped {plural(length(dropped_vars), 'variable')}",
                           " ({format_list(dropped_vars)})"))
        has_changed = TRUE
    }

    for (var in names(newdata)) {
        # new var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            display(glue::glue("{prefix} new variable '{var}' ({get_type(newdata[[var]])}) ",
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

            if (typeold %in% c("factor", "ordered factor") &
                typenew %in% c("factor", "ordered factor")) {
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
                na_text <- plural(abs(new_na), "NA", mid = ifelse(new_na >= 0, "new ", "fewer "))
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
