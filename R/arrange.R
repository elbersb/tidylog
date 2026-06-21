# Logger for dplyr::arrange(). Handles the following argument types in `...`:
#
# Bare columns:          arrange(x, col), arrange(x, col1, col2)
#                        arrange(x, `my col`)
#                        arrange(x, .data$col)
# desc():                arrange(x, desc(col)), arrange(x, dplyr::desc(col))
# across()/pick():       arrange(x, across(everything()))
#                        arrange(x, across(starts_with("x")))
#                        arrange(x, across(a:b))
#                        arrange(x, pick(a, b))
#                        arrange(x, across(.cols, desc))  - desc as function arg
#                        arrange(x, across())             - deprecated, treated as everything()
#                        arrange(x, pick())               - error, let dplyr handle it
# Tidy-select directly:  arrange(x, c(a, b)), arrange(x, a:b)
# Programmatic:          arrange(x, !!sym(var))
#                        arrange(x, across(all_of(vars)))
#
# NAs in sorting columns sort last in dplyr. Two distinct situations are
# marked in the printed list, each with its own legend line:
#
#   - A column known to contain NAs is suffixed with cli::symbol$sup_1.
#   - A sorting expression whose NA status is unknowable without
#     evaluating it (e.g. `mpg * 2`, `is.na(cyl)`, `my_func(mpg)`) is
#     suffixed with cli::symbol$sup_2. Whether NAs propagate through an
#     arbitrary expression can't be determined from syntax alone
#     (is.na(col) is never itself NA, while col * 2 propagates col's NAs --
#     a fixed rule can't distinguish these for arbitrary functions). These
#     are marked as "may contain NAs" rather than silently treated as
#     NA-free.
#
# Note: on non-UTF8 terminals, cli falls back to plain digits ("1"/"2")
# for these symbols.
log_arrange <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) || !should_display()) {
        return()
    }

    is_grouped <- dplyr::is_grouped_df(.olddata)
    grp_prefix <- if (is_grouped) " (grouped)" else ""
    grp_infix <- if (is_grouped) " within groups" else ""

    prefix <- glue::glue("{.funname}{grp_prefix}:")

    # If no changes, stop here.
    if (identical(.olddata, .newdata)) {
        display(glue::glue("{prefix} no changes"))
        return()
    }

    # Capture as quosures to preserve the user's environment
    vars <- rlang::enquos(...)

    # Named arguments to arrange() that are options, not sorting columns.
    # Update if dplyr adds new named arguments to arrange().
    to_exclude <- c(".by_group", ".locale")
    vars <- vars[!names(vars) %in% to_exclude]

    processed <- lapply(
        vars,
        function(q) process_arrange_var(q, data = .olddata)
    )

    # Determine which bare columns (across all processed args) contain NAs,
    # so we can mark their labels below.
    all_cols <- unique(unlist(lapply(processed, `[[`, "cols")))
    valid_cols <- intersect(all_cols, names(.olddata))
    na_cols <- if (length(valid_cols) > 0) {
        tryCatch(
            names(which(vapply(.olddata[valid_cols], anyNA, FUN.VALUE = logical(1)))),
            error = function(err) character(0)
        )
    } else {
        character(0)
    }

    sup_1 <- cli::symbol$sup_1
    sup_2 <- cli::symbol$sup_2

    # Compute each label's marker per entry:
    #   - no labels -> no marker
    #   - no cols to check because the input is complex (e.g. `mpg * 2` or
    #       `is.na(hp)`) and thus has unknowable NA state -> sup_2
    #   - otherwise sup_1 if the column has NAs
    markers_per_entry <- lapply(processed, function(p) {
        n <- length(p$labels)
        if (n == 0) {
            character(0)
        } else if (length(p$cols) == 0) {
            rep(sup_2, n)
        } else if (length(p$cols) != n) {
            stop("process_arrange_var() invariant violated: cols length doesn't match labels length")
        } else {
            ifelse(p$cols %in% na_cols, sup_1, "")  # "" = no marker
        }
    })

    all_labels <- unlist(lapply(processed, `[[`, "labels"))
    markers <- unlist(markers_per_entry)

    # Dedup labels+markers together, keeping first occurrence. A label's
    # marker is always the same wherever it recurs, so this is safe.
    keep <- !duplicated(all_labels)
    all_labels <- all_labels[keep]
    markers <- markers[keep]

    all_labels_marked <- format_list(all_labels, markers = markers)
    display(glue::glue(
        "{prefix} sorted rows{grp_infix} by {all_labels_marked}"
    ))

    ws_pre <- replace_with_ws(prefix)

    if (sup_1 %in% markers) {
        display(glue::glue(
            "{ws_pre} {sup_1}contains NAs, which sort to the end"
        ))
    }

    if (sup_2 %in% markers) {
        display(glue::glue(
            "{ws_pre} {sup_2}NA status unknown"
        ))
    }
}

# Evaluates a column selection expression against data, returning matched column names.
# Handles tidyselect helpers (everything(), starts_with(), all_of(), c(), a:b, etc.)
# Does not automatically unpack across() and pick(), for example.
# Returns character(0) if the expression is not a valid column selection.
resolve_tidyselect <- function(expr, data, env) {
    # Must pass the quosure environment down to ensure that the correct
    # variable scope is available, e.g. for `all_of(vars)`.
    tryCatch(
        names(tidyselect::eval_select(expr, data, env = env)),
        error = function(err) character(0)
    )
}

# Process pick(...) expressions
process_pick <- function(e, data, env) {
    args <- rlang::call_args(e)

    if (length(args) == 0) {
        # pick() with no args is a dplyr error: return nothing and
        # let dplyr surface the error from the arrange() call itself
        return(list(labels = character(0), cols = character(0)))
    }

    inner <- rlang::expr(c(!!!args))
    resolved <- resolve_tidyselect(inner, data, env)
    list(labels = resolved, cols = resolved)
}

# Process across(.cols, .fn) expressions
process_across <- function(e, data, env) {
    args <- rlang::call_args(e)

    inner <- if (length(args) == 0) {
        # across() with no args resolves to everything (though deprecated)
        quote(everything())
    } else {
        args[[1]]
    }

    resolved <- resolve_tidyselect(inner, data, env)

    has_desc_fn <- length(args) >= 2 &&
        rlang::expr_text(args[[2]]) %in% c("desc", "dplyr::desc")

    labels <- if (has_desc_fn) paste0("desc(", resolved, ")") else resolved

    list(labels = labels, cols = resolved)
}

# Process desc() expressions into labels and cols
# Since desc() takes a data-masking argument, we delegate to process_arrange_var
# to handle complex inner expressions recursively, e.g. desc(across(...)).
process_desc <- function(e, data, env) {
    inner <- rlang::call_args(e)[[1]]
    inner_q <- rlang::new_quosure(inner, env)
    inner_processed <- process_arrange_var(inner_q, data)
    list(
        labels = paste0("desc(", inner_processed$labels, ")"),
        cols = inner_processed$cols
    )
}

# Process use of the `$` operator. If `.data$some_col`, extract the column for
# NA processing. Otherwise, mark unknown.
process_dollar <- function(e, data, env) {
    args <- as.character(rlang::call_args(e))
    if (args[[1]] == ".data" && args[[2]] %in% colnames(data)) {
        col <- args[[2]]
        list(labels = col, cols = col)
    } else {
        list(labels = rlang::expr_text(e), cols = character(0))
    }
}

# Formats with backticks if non-syntactic.
format_syntactic <- function(x) {
    # Use make.names to determine if not syntactic.
    ifelse(x != make.names(x), glue::glue("`{x}`"), x)
}

# Returns list(labels = chr vector, cols = chr vector)
# labels: what to display; cols: bare column names for NA checking,
# aligned with labels when both are non-empty. labels non-empty but cols
# empty means NA status is unknowable (see log_arrange).
process_arrange_var <- function(q, data) {
    # Extract the expression and environment
    e <- rlang::quo_get_expr(q)
    env <- rlang::quo_get_env(q)

    # 1. Bare symbols (e.g., col)
    # Note: injected symbols like !!sym("var") are automatically evaluated
    # by enquos() into bare symbols by the time they reach here.
    if (rlang::is_symbol(e)) {
        col <- as.character(e)
        return(list(labels = format_syntactic(col), cols = col))
    }

    # 2. Non-calls: list as-is, cols empty (unknowable NA status).
    if (!rlang::is_call(e)) {
        return(list(labels = rlang::expr_text(e), cols = character(0)))
    }

    # 3. Special cases: across(), pick(), desc()
    fn <- rlang::call_name(e)  # Note: this already removes the `dplyr::` prefix
    if (fn == "across") return(process_across(e, data, env))
    if (fn == "pick") return(process_pick(e, data, env))
    if (fn == "desc") return(process_desc(e, data, env))
    if (fn == "$") return(process_dollar(e, data, env))

    # 4. Any other data-masking expression (e.g. col * 2, is.na(col)):
    # show as-is, cols empty (NA propagation unknowable without evaluating).
    list(labels = rlang::expr_text(e), cols = character(0))
}
