.onAttach <- function(libname, pkgname) {
    # Only update formals when tidylog is attached to avoid overwriting other locals.
    update_tidylog_formals(globalenv())
}
