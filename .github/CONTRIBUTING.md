# Contributing to tidylog

Adding a new dplyr/tidyr function takes about 30 seconds — edit one list, source one file, done.

---

## How to Add a Function

### 1. Add it to the mapping in `tools/generate_wrappers.R`

```r
regular_wrappers <- list(
    log_filter = c(
        "dplyr::filter",
        "dplyr::distinct",
        "dplyr::new_function"  # <- add here
    ),
    ...
)
```

### 2. Regenerate and test

```r
source("tools/generate_wrappers.R")
devtools::test()
```

Commit the modified `R/z_generated_*.R` file(s).

---

## How to Add a New Logger Category

If you need a new type of logging (beyond filter, mutate, select, etc.):

1. Create `R/<category>.R` with a `log_<category>(.olddata, .newdata, .funname, ...)` function
2. Add it to `regular_wrappers` (or `join_wrappers`) in `tools/generate_wrappers.R`
3. Run `source("tools/generate_wrappers.R")` and `devtools::test()`

---

## Pinned Versions

`WRAPPER_DOC_VERSIONS` in `tools/generate_wrappers.R` pins the dplyr/tidyr versions used to generate wrapper documentation. This does **not** affect which versions end users can install. If your installed versions don't match, you'll get a clear error — either install the pinned versions or update the constant and commit that change intentionally.

---

## Architecture

Tidylog uses a two-layer system: **loggers** (`R/filter.R`, `R/join.R`, etc.) handle the analysis and display; **wrappers** (`R/z_generated_*.R`) are thin user-facing functions that call the underlying dplyr/tidyr function and pass results to the logger. Wrappers are generated at build time using `@inheritParams`/`@inheritDotParams` for full RStudio autocomplete support.

`tools/generate_wrappers.R` is the entry point (configuration + trigger). `tools/generate_wrappers_impl.R` is the backend — **do not source it directly**.

The CI workflow (`.github/workflows/check-wrappers.yaml`) verifies that committed wrappers match the generator output.

> [!WARNING]
> **Never edit `R/z_generated_*.R` files manually** — they will be overwritten on the next run.

---

## Questions?

Open an issue on GitHub — we're happy to help!
