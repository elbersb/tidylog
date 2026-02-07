#' Update the function signature for tidylog wrappers
#'
#' @param envir Environment in which to update the formals.
#'
#' @description
#' After calling this function, the function signature for tidylog wrappers is
#' updated to the original function that it wraps. This improves tab
#' auto-completion, at the expense of some traceability.
#'
#' @export
update_tidylog_formals <- function(envir = parent.frame()) {

    #----- filter.R -----
    formals(filter, envir) <- formals(dplyr::filter)
    formals(filter_all, envir) <- formals(dplyr::filter_all)
    formals(filter_if, envir) <- formals(dplyr::filter_if)
    formals(filter_at, envir) <- formals(dplyr::filter_at)
    formals(distinct, envir) <- formals(dplyr::distinct)
    formals(distinct_all, envir) <- formals(dplyr::distinct_all)
    formals(distinct_if, envir) <- formals(dplyr::distinct_if)
    formals(distinct_at, envir) <- formals(dplyr::distinct_at)
    formals(top_n, envir) <- formals(dplyr::top_n)
    formals(top_frac, envir) <- formals(dplyr::top_frac)
    formals(sample_n, envir) <- formals(dplyr::sample_n)
    formals(sample_frac, envir) <- formals(dplyr::sample_frac)
    formals(slice, envir) <- formals(dplyr::slice)
    formals(slice_head, envir) <- formals(dplyr::slice_head)
    formals(slice_tail, envir) <- formals(dplyr::slice_tail)
    formals(slice_min, envir) <- formals(dplyr::slice_min)
    formals(slice_max, envir) <- formals(dplyr::slice_max)
    formals(slice_sample, envir) <- formals(dplyr::slice_sample)
    formals(drop_na, envir) <- formals(tidyr::drop_na)

    #----- group_by.R -----
    formals(group_by, envir) <- formals(dplyr::group_by)
    formals(group_by_all, envir) <- formals(dplyr::group_by_all)
    formals(group_by_if, envir) <- formals(dplyr::group_by_if)
    formals(group_by_at, envir) <- formals(dplyr::group_by_at)
    formals(ungroup, envir) <- formals(dplyr::ungroup)

    #----- join.R -----
    formals(inner_join, envir) <- formals(dplyr::inner_join)
    formals(full_join, envir) <- formals(dplyr::full_join)
    formals(left_join, envir) <- formals(dplyr::left_join)
    formals(right_join, envir) <- formals(dplyr::right_join)
    formals(anti_join, envir) <- formals(dplyr::anti_join)
    formals(semi_join, envir) <- formals(dplyr::semi_join)

    #----- longer_wider.R -----
    formals(pivot_longer, envir) <- formals(tidyr::pivot_longer)
    formals(pivot_wider, envir) <- formals(tidyr::pivot_wider)
    formals(gather, envir) <- formals(tidyr::gather)
    formals(spread, envir) <- formals(tidyr::spread)
    formals(separate_wider_position, envir) <- formals(tidyr::separate_wider_position)
    formals(separate_wider_delim, envir) <- formals(tidyr::separate_wider_delim)
    formals(separate_wider_regex, envir) <- formals(tidyr::separate_wider_regex)

    #----- mutate.R -----
    formals(mutate, envir) <- formals(dplyr::mutate)
    formals(mutate_all, envir) <- formals(dplyr::mutate_all)
    formals(mutate_if, envir) <- formals(dplyr::mutate_if)
    formals(mutate_at, envir) <- formals(dplyr::mutate_at)
    formals(transmute, envir) <- formals(dplyr::transmute)
    formals(transmute_all, envir) <- formals(dplyr::transmute_all)
    formals(transmute_if, envir) <- formals(dplyr::transmute_if)
    formals(transmute_at, envir) <- formals(dplyr::transmute_at)
    formals(add_count, envir) <- formals(dplyr::add_count)
    formals(add_tally, envir) <- formals(dplyr::add_tally)
    formals(replace_na, envir) <- formals(tidyr::replace_na)
    formals(fill, envir) <- formals(tidyr::fill)

    #----- rename.R -----
    formals(rename, envir) <- formals(dplyr::rename)
    formals(rename_all, envir) <- formals(dplyr::rename_all)
    formals(rename_if, envir) <- formals(dplyr::rename_if)
    formals(rename_at, envir) <- formals(dplyr::rename_at)
    formals(rename_with, envir) <- formals(dplyr::rename_with)

    #----- select.R -----
    formals(select, envir) <- formals(dplyr::select)
    formals(select_all, envir) <- formals(dplyr::select_all)
    formals(select_if, envir) <- formals(dplyr::select_if)
    formals(select_at, envir) <- formals(dplyr::select_at)
    formals(relocate, envir) <- formals(dplyr::relocate)

    #----- summarize.R -----
    formals(summarize, envir) <- formals(dplyr::summarize)
    formals(summarize_all, envir) <- formals(dplyr::summarize_all)
    formals(summarize_at, envir) <- formals(dplyr::summarize_at)
    formals(summarize_if, envir) <- formals(dplyr::summarize_if)
    formals(summarise, envir) <- formals(dplyr::summarise)
    formals(summarise_all, envir) <- formals(dplyr::summarise_all)
    formals(summarise_at, envir) <- formals(dplyr::summarise_at)
    formals(summarise_if, envir) <- formals(dplyr::summarise_if)
    formals(tally, envir) <- formals(dplyr::tally)
    formals(count, envir) <- formals(dplyr::count)
    formals(uncount, envir) <- formals(tidyr::uncount)
}


