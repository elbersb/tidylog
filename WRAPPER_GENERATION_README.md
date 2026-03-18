# Wrapper Generation System

This document describes how tidylog's wrapper functions are generated at build time to enable RStudio autocomplete while maintaining clean separation between function execution and logging.

## Architecture Overview

Tidylog wraps dplyr/tidyr functions to add logging. The wrapper generation system uses a build-time approach where:

1. **Wrappers execute functions** - Simple wrapper functions call the underlying dplyr/tidyr function
2. **Loggers perform logging** - Separate logger functions analyze and display the results
3. **Generated at build time** - All wrappers are created by running `data-raw/generate_wrappers.R`
4. **Committed to git** - Generated files in `R/z_generated_*.R` are version controlled

## Key Files

### `data-raw/wrapper_mapping.R`
Defines which functions should be wrapped and which logger handles them:

```r
regular_wrappers <- list(
    log_filter = c("dplyr::filter", "dplyr::distinct", "dplyr::slice", ...),
    log_mutate = c("dplyr::mutate", "dplyr::transmute", ...),
    ...
)

join_wrappers <- list(
    log_join = c("dplyr::left_join", "dplyr::inner_join", ...)
)
```

### `data-raw/generate_wrappers.R`
Contains two generator functions:

- `generate_regular_wrapper()` - Creates wrappers with signature `function(.data, ...)`
- `generate_join_wrapper()` - Creates wrappers with signature `function(x, y, by = NULL, ...)`

Running this script:
1. Removes all existing `R/z_generated_*.R` files
2. Generates new wrapper files
3. Runs `devtools::document()` to update documentation

## Generated Wrapper Pattern

### Example: dplyr::filter

```r
#' Wrapper around dplyr::filter that prints information about the operation
#'
#' @description
#' Wrapper around [dplyr::filter()] that prints information about the operation.
#'
#' @details
#' Documentation generated from dplyr version 1.2.0.
#'
#' @inheritParams dplyr::filter
#' @inheritDotParams dplyr::filter
#'
#' @return See [dplyr::filter()]
#' @seealso [dplyr::filter()]
#' @export
filter <- function(.data, ...) {
    result <- dplyr::filter(.data, ...)
    log_filter(.olddata = .data, .newdata = result, .funname = "filter")
    result
}
```

## Logger Function Signatures

Loggers are void functions that only perform logging:

### Regular Loggers
```r
log_filter(.olddata, .newdata, .funname, ...)
log_mutate(.olddata, .newdata, .funname, ...)
log_select(.olddata, .newdata, .funname, ...)
# ... etc
```

### Join Logger
```r
log_join(x, y, by, .newdata, .funname, ...)
```

The `...` parameter is preserved for special cases (e.g., `slice_min`/`slice_max` need to inspect `with_ties` argument).

## RStudio Autocomplete

The `@inheritDotParams` roxygen tag enables autocomplete by inheriting parameter documentation from the underlying dplyr/tidyr functions. This works with RStudio's autocomplete system (requires RStudio PR #17149 or later).

## Process to Add/Update Wrappers

### Adding a New Function

1. Add the function to the appropriate list in `data-raw/wrapper_mapping.R`:
```r
regular_wrappers <- list(
    log_filter = c(
        "dplyr::filter",
        "dplyr::new_function"  # Add here
    )
)
```

2. Regenerate wrappers:
```r
source("data-raw/generate_wrappers.R")
```

3. Commit the new/modified `R/z_generated_*.R` file(s)

### Adding a New Logger Category

1. Create the logger function in `R/log_<category>.R`:
```r
log_<category> <- function(.olddata, .newdata, .funname, ...) {
    # Logging logic here
}
```

2. Add a new list in `data-raw/wrapper_mapping.R`:
```r
regular_wrappers <- list(
    log_<category> = c("pkg::function1", "pkg::function2")
)
```

3. Regenerate wrappers as above

### Modifying Wrapper Generation Logic

If you need to change how wrappers are generated:

1. Edit `generate_regular_wrapper()` or `generate_join_wrapper()` in `data-raw/generate_wrappers.R`
2. Regenerate all wrappers by running the script
3. Review the git diff to ensure changes are correct
4. Commit all modified `R/z_generated_*.R` files

## File Naming Convention

Generated files are named `R/z_generated_<logger_name>.R` (e.g., `R/z_generated_log_filter.R`). 

The `z_generated_` prefix serves two purposes:
1. **Load order**: The `z_` prefix ensures these files load after the logger function definitions
2. **Tracking**: The `generated_` portion clearly identifies programmatically generated files that should not be manually edited

## Benefits of This Approach

- **Autocomplete works**: RStudio can provide parameter suggestions
- **Simple wrappers**: No complex signature matching required
- **Backward compatible**: Works with any dplyr/tidyr version
- **Inspectable**: Generated code is visible in git
- **Clean separation**: Wrappers execute, loggers log
- **Easy maintenance**: Regenerate all wrappers with one command
