unlink("man", recursive = TRUE)
devtools::document(roclets = c('rd', 'collate', 'namespace'))

library(glue)
library(tidylog)
library(dplyr)
library(tidyr)

# get tidylog functions (not very elegant...)
conn <- file("NAMESPACE")
namespace <- readLines(conn)
close(conn)
functions <- namespace[grepl("export", namespace)]
functions <- sub("export\\(", "", functions)
functions <- sub("\\)", "", functions)

functions_dplyr <- lsf.str("package:dplyr")
functions_tidyr <- lsf.str("package:tidyr")

template <- "\\name{<f>}
\\alias{<f>}
\\title{Wrapper around <package>::<f>
that prints information about the operation}
\\usage{
<usage>
}
\\arguments{
<args>
}
\\value{
see \\link[<package>]{<f>}
}
\\description{
Wrapper around <package>::<f>
that prints information about the operation
}"

tidylog_env <- environment(tidylog::tidylog)

for (f in functions) {
    if (f %in% functions_dplyr) {
        package <- "dplyr"
    } else if (f %in% functions_tidyr) {
        package <- "tidyr"
    } else {
        next
    }
    args_list <- names(formals(f, tidylog_env))
    usage <- glue("{f}({paste(args_list, collapse = ', ')})")
    args <- glue("\\item{<args_list>}{see \\link[<package>]{<f>}}", .open = "<", .close = ">")
    args <- paste0(args, collapse = "\n\n")

    doc <- glue(template, .open = "<", .close = ">")
    conn <- file(glue("man/{f}.Rd"))
    writeLines(doc, conn)
    close(conn)
}
