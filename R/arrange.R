# Logger for functions that reorders rows
log_arrange <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }

    # add group status
    is_grouped <- dplyr::is_grouped_df(.olddata)
    grp_prefix <- if (is_grouped) " (grouped)" else ""
    grp_suffix <- if (is_grouped) " (within groups)" else ""

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

    cols <- paste0(as.character(vars), collapse=", ")

    display(glue::glue("{prefix} arranged rows by {cols}{grp_suffix}"))
}
