
# markdown helpers --------------------------------------------------------

markdown_appendix <- function (name, content) {
  paste(paste("##", name, "{.appendix}"), " ", content, sep = "\n")
}
markdown_link <- function (text, path) {
  paste0("[", text, "](", path, ")")
}



# worker functions --------------------------------------------------------

insert_source <- function(repo_spec, name,
                          collection = "posts",
                          branch = "main",
                          host = "https://github.com",
                          text = "source code") {
  path <- paste(
    host,
    repo_spec,
    "tree",
    branch,
    collection,
    name,
    "index.qmd",
    sep = "/"
  )
  return(markdown_link(text, path))
}

insert_timestamp <- function(tzone = Sys.timezone()) {
  time <- lubridate::now(tzone = tzone)
  stamp <- as.character(time, tz = tzone, usetz = TRUE)
  return(stamp)
}

insert_lockfile <- function(repo_spec, name,
                            collection = "posts",
                            branch = "main",
                            host = "https://github.com",
                            text = "R environment") {
  path <- paste(
    host,
    repo_spec,
    "tree",
    branch,
    collection,
    name,
    "renv.lock",
    sep = "/"
  )
  return(markdown_link(text, path))
}



# top level function ------------------------------------------------------

insert_appendix <- function (repo_spec, name, collection = "posts") {
  appendices <- paste(
    markdown_appendix(
      name = "Last updated",
      content = insert_timestamp()
    ),
    " ",
    markdown_appendix(
      name = "Details",
      content = paste(
        insert_source(repo_spec, name, collection),
        insert_lockfile(repo_spec, name, collection),
        sep = ", ")
    ),
    sep = "\n"
  )
  knitr::asis_output(appendices)
}



# call it -----------------------------------------------------------------

insert_appendix(
  repo_spec = "djnavarro/quarto-blog",
  name = long_slug
)

