#' Displays messages related to changing row number.
#'
#' @description Unlike `log_...()` functions, this assumes the data manipulation
#'  is performed elsewhere and only displays messages.
#'
#' @param .olddata Data frame before transformation.
#' @param .newdata Data frame after transformation.
#' @param .funname String: name of function that should be used in messages.
display_changed_rows <- function(.olddata, .newdata, .funname) {
    if (!"data.frame" %in% class(.olddata)
        | !"data.frame" %in% class(.newdata)
        | !should_display()) {
        return()
    }

    if (dplyr::is.grouped_df(.newdata) == TRUE) {
        group_status <- " (grouped)"
        groups_diff <- dplyr::n_groups(.olddata) - dplyr::n_groups(.newdata)
        group_desc <- glue::glue(" (removed {plural(groups_diff, 'group')}, {plural(dplyr::n_groups(.newdata), 'group')} remaining)")
    } else {
        group_status <- ""
        group_desc <- ""
    }

    n <- nrow(.olddata) - nrow(.newdata)
    if (n == 0) {
        display(glue::glue("{.funname}{group_status}: no rows removed"))
    } else if (n == nrow(.olddata)) {
        display(glue::glue("{.funname}{group_status}: removed all rows (100%)"))
    } else {
        total <- nrow(.olddata)
        display(glue::glue("{.funname}{group_status}: ",
                           "removed {plural(n, 'row')} ",
                           "({percent(n, {total})}), {plural(nrow(.newdata), 'row')} remaining{group_desc}"))
    }
}
