#' Wrapper around dplyr::filter
#'
#' @param .data a tbl; see \link[dplyr]{filter}
#' @param ... see \link[dplyr]{filter}
#' @return see \link[dplyr]{filter}
#' @import dplyr
#' @export
filter <- function(.data, ...) {
    dropped <- dplyr::filter(.data, ...)
    p <- round(100 * (1 - nrow(dropped) / nrow(.data)))
    cat(paste0("filter: removed ", nrow(.data) - nrow(dropped), " rows", " (", p, "%)\n"))
    dropped
}
