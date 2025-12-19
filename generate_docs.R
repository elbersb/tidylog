unlink("man", recursive = TRUE)
devtools::document(roclets = c('rd', 'collate', 'namespace'))

library(glue)
library(tidylog)
library(dplyr)
library(tidyr)
library(stringr)

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
see \\link[<package><topic>]{<f>}
}
\\description{
Wrapper around <package>::<f>
that prints information about the operation
}"

tidylog_env <- environment(tidylog::tidylog)
html_links <- tools::findHTMLlinks(pkgDir = "", level = 0:5)

for (f in functions) {
    if (f %in% functions_dplyr) {
        package <- "dplyr"
    } else if (f %in% functions_tidyr) {
        package <- "tidyr"
    } else {
        next
    }
    # find topic
    topic <- html_links[str_detect(html_links, package) & names(html_links) == f]
    topic <- str_match(topic, "/([^/]*)\\.html")[, 2]
    if (length(topic) == 0) {
        topic <- ""
    } else {
        topic <- paste0(":", topic)
    }

    args_list <- names(formals(f, tidylog_env))
    usage <- glue("{f}({paste(args_list, collapse = ', ')})")
    args <- glue("\\item{<args_list>}{see \\link[<package><topic>]{<f>}}", .open = "<", .close = ">")
    args <- paste0(args, collapse = "\n\n")

    doc <- glue(template, .open = "<", .close = ">")
    conn <- file(glue("man/{f}.Rd"))
    writeLines(doc, conn)
    close(conn)
}
