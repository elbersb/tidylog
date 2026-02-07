#' Update the function signature for tidylog wrappers
#'
#' @description
#' After calling this function, the function signature for tidylog wrappers is
#' updated to the original function that it wraps. This improves tab
#' auto-completion, at the expense of some traceability.
update_tidylog_formals <- function() {
    #----- filter.R -----
    formals(filter) <- formals(dplyr::filter)
    formals(filter_all) <- formals(dplyr::filter_all)
    formals(filter_if) <- formals(dplyr::filter_if)
    formals(filter_at) <- formals(dplyr::filter_at)
    formals(distinct) <- formals(dplyr::distinct)
    formals(distinct_all) <- formals(dplyr::distinct_all)
    formals(distinct_if) <- formals(dplyr::distinct_if)
    formals(distinct_at) <- formals(dplyr::distinct_at)
    formals(top_n) <- formals(dplyr::top_n)
    formals(top_frac) <- formals(dplyr::top_frac)
    formals(sample_n) <- formals(dplyr::sample_n)
    formals(sample_frac) <- formals(dplyr::sample_frac)
    formals(slice) <- formals(dplyr::slice)
    formals(slice_head) <- formals(dplyr::slice_head)
    formals(slice_tail) <- formals(dplyr::slice_tail)
    formals(slice_min) <- formals(dplyr::slice_min)
    formals(slice_max) <- formals(dplyr::slice_max)
    formals(slice_sample) <- formals(dplyr::slice_sample)
    formals(drop_na) <- formals(tidyr::drop_na)

    #----- group_by.R -----
    formals(group_by) <- formals(dplyr::group_by)
    formals(group_by_all) <- formals(dplyr::group_by_all)
    formals(group_by_if) <- formals(dplyr::group_by_if)
    formals(group_by_at) <- formals(dplyr::group_by_at)
    formals(ungroup) <- formals(dplyr::ungroup)

    #----- join.R -----
    formals(inner_join) <- formals(dplyr::inner_join)
    formals(full_join) <- formals(dplyr::full_join)
    formals(left_join) <- formals(dplyr::left_join)
    formals(right_join) <- formals(dplyr::right_join)
    formals(anti_join) <- formals(dplyr::anti_join)
    formals(semi_join) <- formals(dplyr::semi_join)

    #----- longer_wider.R -----
    formals(pivot_longer) <- formals(tidyr::pivot_longer)
    formals(pivot_wider) <- formals(tidyr::pivot_wider)
    formals(gather) <- formals(tidyr::gather)
    formals(spread) <- formals(tidyr::spread)
    formals(separate_wider_position) <- formals(tidyr::separate_wider_position)
    formals(separate_wider_delim) <- formals(tidyr::separate_wider_delim)
    formals(separate_wider_regex) <- formals(tidyr::separate_wider_regex)

    #----- mutate.R -----
    formals(mutate) <- formals(dplyr::mutate)
    formals(mutate_all) <- formals(dplyr::mutate_all)
    formals(mutate_if) <- formals(dplyr::mutate_if)
    formals(mutate_at) <- formals(dplyr::mutate_at)
    formals(transmute) <- formals(dplyr::transmute)
    formals(transmute_all) <- formals(dplyr::transmute_all)
    formals(transmute_if) <- formals(dplyr::transmute_if)
    formals(transmute_at) <- formals(dplyr::transmute_at)
    formals(add_count) <- formals(dplyr::add_count)
    formals(add_tally) <- formals(dplyr::add_tally)
    formals(replace_na) <- formals(tidyr::replace_na)
    formals(fill) <- formals(tidyr::fill)

    #----- rename.R -----
    formals(rename) <- formals(dplyr::rename)
    formals(rename_all) <- formals(dplyr::rename_all)
    formals(rename_if) <- formals(dplyr::rename_if)
    formals(rename_at) <- formals(dplyr::rename_at)
    formals(rename_with) <- formals(dplyr::rename_with)

    #----- select.R -----
    formals(select) <- formals(dplyr::select)
    formals(select_all) <- formals(dplyr::select_all)
    formals(select_if) <- formals(dplyr::select_if)
    formals(select_at) <- formals(dplyr::select_at)
    formals(relocate) <- formals(dplyr::relocate)

    #----- summarize.R -----
    formals(summarize) <- formals(dplyr::summarize)
    formals(summarize_all) <- formals(dplyr::summarize_all)
    formals(summarize_at) <- formals(dplyr::summarize_at)
    formals(summarize_if) <- formals(dplyr::summarize_if)
    formals(summarise) <- formals(dplyr::summarise)
    formals(summarise_all) <- formals(dplyr::summarise_all)
    formals(summarise_at) <- formals(dplyr::summarise_at)
    formals(summarise_if) <- formals(dplyr::summarise_if)
    formals(tally) <- formals(dplyr::tally)
    formals(count) <- formals(dplyr::count)
    formals(uncount) <- formals(tidyr::uncount)
}


