library(glue)
library(purrr)

wrapper_mapping <- list(
    log_filter = c(
        "dplyr::filter", "dplyr::filter_all", "dplyr::filter_if", "dplyr::filter_at",
        "dplyr::distinct", "dplyr::distinct_all", "dplyr::distinct_if", "dplyr::distinct_at",
        "dplyr::top_n", "dplyr::top_frac",
        "dplyr::sample_n", "dplyr::sample_frac",
        "dplyr::slice",
        "dplyr::slice_head", "dplyr::slice_tail",
        "dplyr::slice_min", "dplyr::slice_max",
        "dplyr::slice_sample",
        "tidyr::drop_na"
    ),
    log_join = c(
        "dplyr::inner_join",
        "dplyr::full_join",
        "dplyr::left_join",
        "dplyr::right_join",
        "dplyr::anti_join",
        "dplyr::semi_join"
    ),

    log_mutate = c(
        "dplyr::mutate", "dplyr::mutate_all", "dplyr::mutate_if", "dplyr::mutate_at",
        "dplyr::transmute", "dplyr::transmute_all", "dplyr::transmute_if", "dplyr::transmute_at",
        "dplyr::add_tally",
        "dplyr::add_count",
        "tidyr::replace_na",
        "tidyr::fill"
    ),

    log_select = c(
        "dplyr::select", "dplyr::select_all", "dplyr::select_if", "dplyr::select_at",
        "dplyr::relocate"
    ),

    log_summarize = c(
        "dplyr::summarize", "dplyr::summarize_all", "dplyr::summarize_at", "dplyr::summarize_if",
        "dplyr::summarise", "dplyr::summarise_all", "dplyr::summarise_at", "dplyr::summarise_if",
        "dplyr::tally",
        "dplyr::count",
        "tidyr::uncount"
    ),

    log_group_by = c(
        "dplyr::group_by", "dplyr::group_by_all", "dplyr::group_by_at", "dplyr::group_by_if",
        "dplyr::ungroup"),
    log_rename   = c(
        "dplyr::rename", "dplyr::rename_all", "dplyr::rename_if", "dplyr::rename_at",
        "dplyr::rename_with"),
    log_longer_wider = c(
        "tidyr::pivot_longer",
        "tidyr::pivot_wider",
        "tidyr::gather",
        "tidyr::spread",
        "tidyr::separate_wider_delim",
        "tidyr::separate_wider_position",
        "tidyr::separate_wider_regex")
)

get_fn_parts <- function(full_fn_name) {
    strsplit(full_fn_name, "::")[[1]]
}

# 2. Validation Step (Catch typos here)
# ------------------------------------------------------------------------------
# This loop ensures every function exists before we even try to generate code.
walk(
    unlist(wrapper_mapping),
    function(full_name) {
        parts <- get_fn_parts(full_name)
        if (!exists(parts[2], where = asNamespace(parts[1]), mode = "function")) {
            stop(glue("Typo detected: '{full_name}' does not exist!"))
        }
    }
)
