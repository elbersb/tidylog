# ==============================================================================
# NOTE: Do not source this file directly.
# This file is sourced automatically by tools/generate_wrappers.R
# ==============================================================================

# Suppress messages here so that warnings from check_dep_versions() are not lost in the mix.
suppressMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(glue)
})


# Helpers -----------------------------------------------------------------

# Helper to parse "package::function" into named components
parse_function_name <- function(full_name) {
    parts <- strsplit(full_name, "::")[[1]]
    list(pkg = parts[1], fn = parts[2])
}

# Generated file naming convention
GENERATED_FILE_PREFIX <- "z_generated_"

# Build the file path for a generated wrapper file
build_file_path <- function(logger) {
    paste0("R/", GENERATED_FILE_PREFIX, logger, ".R")
}


# Check that the current versions match the pinned versions before proceeding ---------------
source("tools/versions.R", local = TRUE)

check_dep_versions <- function() {
    mismatches <- imap_chr(WRAPPER_DOC_VERSIONS, function(pinned, pkg) {
        installed <- as.character(packageVersion(pkg))
        if (installed != pinned) {
            sprintf("%s: installed=%s, pinned=%s", pkg, installed, pinned)
        } else {
            NA_character_
        }
    }) |> discard(is.na)

    if (length(mismatches) > 0) {
        stop(paste0(
            "Version mismatch with pinned versions:\n  ",
            paste(mismatches, collapse = "\n  "),
            "\nInstall the pinned versions or update WRAPPER_DOC_VERSIONS in tools/generate_wrappers.R."
        ))
    }
}

check_dep_versions()




# Validation: Ensure all functions defined actually exist -----------

walk(
    c(
        unlist(regular_wrappers),
        unlist(join_wrappers)
    ),
    function(full_name) {
        parts <- parse_function_name(full_name)
        if (!exists(parts$fn, where = asNamespace(parts$pkg), mode = "function")) {
            stop(glue("Typo detected: '{full_name}' does not exist!"))
        }
    }
)



# Generate new files ------------------------------------------------------

# Clean up old generated files before creating new ones
old_generated_files <- list.files("R", pattern = paste0("^", GENERATED_FILE_PREFIX, ".*\\.R$"), full.names = TRUE)
if (length(old_generated_files) > 0) {
    file.remove(old_generated_files)
    message("Removed ", length(old_generated_files), " old generated file(s)")
}

# Shared roxygen documentation template
generate_roxygen_header <- function(pkg, fn, wrapper_params = ".data") {
    pkg_version <- as.character(packageVersion(pkg))

    # Use documented params rather than formals, matching how roxygen2 works.
    # This correctly picks up params documented on S3 methods (e.g. .by/.keep
    # on mutate.data.frame) that don't appear in the generic's formals.
    rd <- roxygen2:::get_rd_from_help(package = pkg, alias = fn, source = fn)
    documented_params <- names(roxygen2:::topic_params(rd))

    # Exclude both wrapper params and ... from the check
    additional_params <- setdiff(documented_params, c(wrapper_params, "..."))
    has_dot_params <- length(additional_params) > 0

    # Build inheritDotParams line conditionally
    inherit_dot_params <- if (has_dot_params) {
        glue("@inheritDotParams {pkg}::{fn}")
    } else {
        ""
    }

    glue("
#' Wrapper around {pkg}::{fn} that prints information about the operation
#'
#' @description
#' Wrapper around [{pkg}::{fn}()] that prints information about the operation.
#'
#' @details
#' Documentation generated from {pkg} version {pkg_version}.
#'
#' @inheritParams {pkg}::{fn}
#' {inherit_dot_params}
#'
#' @return See [{pkg}::{fn}()]
#' @seealso [{pkg}::{fn}()]
#' @export", .trim = FALSE)
}

# Generate wrapper for regular functions: function(.data, ...)
generate_regular_wrapper <- function(full_fn_name, logger_name) {
    parts <- parse_function_name(full_fn_name)

    # Get first argument name from original function
    orig_fn <- getExportedValue(parts$pkg, parts$fn)
    first_arg <- names(formals(orig_fn))[1]

    roxygen <- generate_roxygen_header(parts$pkg, parts$fn, wrapper_params = first_arg)

    glue("
{roxygen}
{parts$fn} <- function({first_arg}, ...) {{
\tresult <- {parts$pkg}::{parts$fn}({first_arg}, ...)
\t{logger_name}({first_arg}, result, \"{parts$fn}\", ...)
\tresult
}}
", .trim = FALSE)
}

# Generate wrapper for join functions: function(x, y, by = NULL, ...)
generate_join_wrapper <- function(full_fn_name, logger_name) {
    parts <- parse_function_name(full_fn_name)

    roxygen <- generate_roxygen_header(parts$pkg, parts$fn, wrapper_params = c("x", "y", "by"))

    glue("
{roxygen}
{parts$fn} <- function(x, y, by = NULL, ...) {{
\tresult <- {parts$pkg}::{parts$fn}(x, y, by = by, ...)
\t{logger_name}(x, y, by, result, \"{parts$fn}\",
\t              .name_x = deparse1(substitute(x)),
\t              .name_y = deparse1(substitute(y)), ...)
\tresult
}}
", .trim = FALSE)
}

# Save to disk
header_base <- "# Generated by tools/generate_wrappers.R: do not edit by hand\n"

# Map of logger names to global variable declarations
globals_map <- list(
    log_longer_wider = "\nutils::globalVariables(c('name', 'value'))\n"
)

# Helper function to generate and write wrapper files
generate_and_write <- function(wrappers, generator) {
    iwalk(wrappers, function(fns, logger) {
        file_path <- build_file_path(logger)

        code_blocks <- map_chr(fns, ~generator(.x, logger))

        # Add global variables if defined for this logger
        globals <- globals_map[[logger]] %||% ""

        writeLines(
            c(header_base,
              glue("# Logger category: {logger}\n"),
              code_blocks,
              globals),
            file_path
        )
        message("Generated: ", file_path)
    })
}

# Generate all wrappers
generate_and_write(regular_wrappers, generate_regular_wrapper)
generate_and_write(join_wrappers, generate_join_wrapper)

# Update documentation
devtools::document()
