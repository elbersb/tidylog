#' outputs some information about the data frame/tbl
#'
#' @param .data a tbl/data frame
#' @return same as .data
#' @examples
#' tidylog(mtcars)
#' #> tidylog: data.frame with 32 rows and 11 columns
#' @export
tidylog <- function(.data) {
    if (!"data.frame" %in% class(.data) | !should_display()) {
        return(.data)
    }

    if ("grouped_df" %in% class(.data)) {
        type <- glue::glue("grouped tibble")
    } else if ("tbl" %in% class(.data)) {
        type <- "tibble"
    } else if ("data.table" %in% class(.data)) {
        type <- "data.table"
    } else {
        type <- "data.frame"
    }

    display(glue::glue("tidylog: {type} with {plural(nrow(.data), 'row')} and ",
        "{plural(ncol(.data), 'column')}"))
    .data
}
