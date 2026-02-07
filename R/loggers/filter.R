# Logging of functions that change number of rows, such as dplyr::filter.
# See data-raw/generate_
log_filter <- function(.data, .fun, .funname, ...) {
    newdata <- .fun(.data, ...)
    display_changed_rows(.data, newdata, .funname)
    newdata
}


