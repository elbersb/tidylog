# Contributing to tidylog

Thank you for your interest in contributing to tidylog!

This document explains how tidylog's wrapper generation system works and how to contribute new functions or modify existing ones.

---

## Architecture Overview

Tidylog wraps dplyr/tidyr functions to add logging. The wrapper generation system uses a build-time approach where:

1. **Wrappers execute functions** - Simple wrapper functions call the underlying dplyr/tidyr function
2. **Loggers perform logging** - Separate logger functions analyze and display the results
3. **Generated at build time** - All wrappers are created by running `data-raw/generate_wrappers.R`
4. **Committed to git** - Generated files in `R/z_generated_*.R` are version controlled

### Key Components

**Loggers** (`R/log_*.R`):
- Internal functions that perform the actual logging
- Signatures: `log_filter(.olddata, .newdata, .funname, ...)` for regular functions
- Special case: `log_join(x, y, by, .newdata, .funname, .name_x, .name_y, ...)` for joins

**Wrappers** (`R/z_generated_*.R`):
- User-facing functions with simple signatures: `function(.data, ...)` or `function(x, y, by = NULL, ...)`
- Auto-generated - **never edit these files manually**
- Use `@inheritParams` and `@inheritDotParams` for documentation and RStudio autocomplete

**Configuration** (`data-raw/wrapper_mapping.R`):
- Maps functions to their loggers
- Two lists: `regular_wrappers` and `join_wrappers`

**Generator** (`data-raw/generate_wrappers.R`):
- Creates all wrapper files from the mapping
- Removes old generated files before creating new ones
- Runs `devtools::document()` to update documentation

---

## How to Add a New Function

### 1. Update the Mapping

Add the function to the appropriate list in `data-raw/wrapper_mapping.R`:

```r
regular_wrappers <- list(
    log_filter = c(
        "dplyr::filter",
        "dplyr::distinct",
        "dplyr::new_function"  # Add here
    ),
    ...
)
```

### 2. Regenerate Wrappers

From the package root:

```r
source("data-raw/generate_wrappers.R")
```

This will:
- Delete all existing `R/z_generated_*.R` files
- Generate new wrapper files
- Update documentation via `devtools::document()`

### 3. Test and Commit

```r
devtools::test()
```

Commit the modified/new `R/z_generated_*.R` file(s).

---

## How to Add a New Logger Category

If you need a new type of logging (beyond filter, mutate, select, etc.):

### 1. Create the Logger Function

Create `R/log_<category>.R`:

```r
log_<category> <- function(.olddata, .newdata, .funname, ...) {
    if (!"data.frame" %in% class(.olddata) | !should_display()) {
        return()
    }
    
    # Your logging logic here
    display(glue::glue("{.funname}: your message"))
}
```

### 2. Add to Mapping

In `data-raw/wrapper_mapping.R`:

```r
regular_wrappers <- list(
    log_<category> = c("pkg::function1", "pkg::function2"),
    ...
)
```

### 3. Regenerate and Test

```r
source("data-raw/generate_wrappers.R")
devtools::test()
```

---

## How Generated Wrappers Work

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
    log_filter(.olddata = .data, .newdata = result, .funname = "filter", ...)
    result
}
```

### How Autocomplete Works

The `@inheritDotParams` roxygen tag enables RStudio autocomplete by inheriting parameter documentation from dplyr/tidyr. This requires RStudio PR #17149 or later.

Documentation is generated from the dplyr/tidyr version installed at build time (noted in `@details`).

---

## Logger Function Signatures

### Regular Loggers
```r
log_filter(.olddata, .newdata, .funname, ...)
log_mutate(.olddata, .newdata, .funname, ...)
log_select(.olddata, .newdata, .funname, ...)
# etc.
```

### Join Logger (Special Case)
```r
log_join(x, y, by, .newdata, .funname, .name_x, .name_y, ...)
```

The join logger merges 2 data sets and relies on all three of `x`, `y`, and `by` to determine its logging. It captures variable names to display informative messages (e.g., "rows only in mtcars" instead of "rows only in x").

The `...` parameter is preserved for special cases (e.g., `slice_min`/`slice_max` need to inspect the `with_ties` argument).

---

## File Naming Convention

Generated files are named `R/z_generated_<logger_name>.R` (e.g., `R/z_generated_log_filter.R`).

The `z_generated_` prefix serves two purposes:
1. **Load order**: The `z_` prefix ensures files load after logger function definitions
2. **Tracking**: The `generated_` portion clearly identifies programmatically generated files

---

## Important Guidelines

> [!WARNING]
> **Never edit `R/z_generated_*.R` files manually**
>
> These files are automatically generated. Any manual changes will be overwritten the next time the generator script runs. Always make changes in `data-raw/wrapper_mapping.R` or `data-raw/generate_wrappers.R`.

### Modifying Generator Logic

If you need to change how wrappers are generated:

1. Edit `generate_regular_wrapper()` or `generate_join_wrapper()` in `data-raw/generate_wrappers.R`
2. Regenerate all wrappers: `source("data-raw/generate_wrappers.R")`
3. Review the git diff to ensure changes are correct
4. Commit all modified `R/z_generated_*.R` files

### CI Check

The `.github/workflows/check-wrappers.yaml` workflow ensures generated files stay in sync with the generator code. If it fails, run `source("data-raw/generate_wrappers.R")` locally and commit the changes.

---

## Benefits of This Approach

- **Autocomplete works**: RStudio provides parameter suggestions
- **Simple wrappers**: No complex signature matching required
- **Backward compatible**: Works with any dplyr/tidyr version
- **Inspectable**: Generated code is visible in git
- **Clean separation**: Wrappers execute, loggers log
- **Easy maintenance**: Regenerate all wrappers with one command

---

## Questions?

If you have questions or run into issues, please open an issue on GitHub. We're happy to help!
