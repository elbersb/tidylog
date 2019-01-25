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

log_mutate <- function(.data, fun, funname, ...) {
    cols <- names(.data)
    newdata <- fun(.data, ...)
    if (!"data.frame" %in% class(.data)) {
        return(newdata)
    }

    has_changed <- FALSE
    for (var in names(newdata)) {
        # existing var
        if (!var %in% cols) {
            has_changed <- TRUE
            n <- length(unique(newdata[[var]]))
            p_na <- percent(sum(is.na(newdata[[var]])), length(newdata[[var]]))
            cat(glue::glue("{funname}: new variable '{var}' with {plural(n, 'value', 'unique ')} and {p_na} NA"),
                "\n")
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
                    cat(glue::glue("{funname}: changed {plural(n, 'value')} ({p}) of '{var}', factor levels updated"), "\n")
                } else if (n > 0 & !levels_different) {
                    cat(glue::glue("{funname}: changed {plural(n, 'value')} ({p}) of '{var}'"), "\n")
                } else if (n == 0 & levels_different) {
                    cat(glue::glue("{funname}: factor levels of '{var}' changed"), "\n")
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
                cat(glue::glue("{funname}: changed {plural(n, 'value')} ({p}) of '{var}' ({new_na} new NA)"), "\n")
            } else {
                # different type
                new_na <- sum(is.na(new)) - sum(is.na(old))
                if (new_na == length(new)) {
                    cat(glue::glue("{funname}: converted '{var}' from {typeold} to {typenew} (now 100% NA)"),
                        "\n")
                } else {
                    cat(glue::glue("{funname}: converted '{var}' from {typeold} to {typenew} ({new_na} new NA)"),
                        "\n")
                }
            }
        }
    }

    if (!has_changed) {
        cat(glue::glue("{funname}: no changes"), "\n")
    }
    newdata
}
