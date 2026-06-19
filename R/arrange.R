# Logger for functions that reorders rows
log_arrange <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }

    # add group status
    is_grouped <- dplyr::is_grouped_df(.olddata)
    grp_prefix <- if (is_grouped) " (grouped)" else ""
    grp_infix <- if (is_grouped) " within groups" else ""

    prefix <- glue::glue("{.funname}{grp_prefix}:")

    if(identical(.olddata, .newdata)) {
        display(glue::glue("{prefix} no changes"))
        return()
    }

    vars <- rlang::exprs(...)

    # Ignore these within ... as they are arguments to arrange() rather
    # than sorting columns.
    to_exclude <- c(".by_group", ".locale")
    vars <- vars[!names(vars) %in% to_exclude]

    # Expand across(expr) and pick(expr) -> column names resolved against .olddata
    varnames <- lapply(vars, function(e) {
        if (!rlang::is_call(e) || !rlang::call_name(e) %in% c("across", "pick"))
            return(rlang::expr_text(e))

        # Expand `across(...)` and `pick(...)`.
        # Empty `across()` should be treated as `everything()`, but empty
        # `pick()` is an error.
        inner <- if (length(e) < 2 && rlang::call_name(e) == "across") {
            quote(everything())
        } else {
            e[[2]]
        }

        resolved <- names(tidyselect::eval_select(inner, .olddata))
        as.character(rlang::syms(resolved))
    })
    varnames <- unlist(varnames)

    display(glue::glue("{prefix} sorted rows{grp_infix} by {format_list(varnames)}"))
}
