
# read options for the blog
get_options <- function() {
  list(
    root = rprojroot::find_root(rprojroot::has_file("_targets.R")),
    post = "post",
    site = "_site",
    static = "static"
  )
}

# write footer html
write_footer <- function() {
  cat(
   "<br><br>",
   "<a href=\"../../\">←</a>",
   sep = "\n"
  )
}
