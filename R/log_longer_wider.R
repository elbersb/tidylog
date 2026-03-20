# Logger for functions that reshape the data frame, such as tidyr::pivot_wider.
log_longer_wider <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }

    newcols <- setdiff(names(.newdata), names(.olddata))
    oldcols <- setdiff(names(.olddata), names(.newdata))

    display(glue::glue(
        "{.funname}: ",
        "reorganized ({format_list(oldcols)}) ",
        "into ({format_list(newcols)}) ",
        "[was {nrow(.olddata)}x{ncol(.olddata)}, ",
        "now {nrow(.newdata)}x{ncol(.newdata)}]"
    ))
}
