# functions used by targets to build the blog

# find paths to the rmd files for the blog
page_paths <- function(opt = get_options()) {
  pages <- fs::dir_ls(
    path = fs::path(opt$root, opt$post),
    recurse = TRUE,
    regexp = "[.][rR]?md$"
  )
  unname(unclass(pages))
}

# render a blog post using litedown
fuse_page <- function(page, opt = get_options()) {
  post_output <- litedown::fuse(page)
  site_output <-stringr::str_replace(
    string = post_output,
    pattern = paste0(opt$post, "/"),
    replacement = paste0(opt$site, "/")
  )
  fs::dir_create(fs::path_dir(site_output))
  fs::file_move(post_output, site_output)
}

# copy the static files into the site folder
copy_static <- function(opt = get_options()) {
  fs::dir_create(fs::path(opt$root, opt$site))
  static_files <- fs::dir_ls(fs::path(opt$root, opt$static), all = TRUE)
  fs::file_copy(
    static_files,
    static_files |> stringr::str_replace(
      pattern = opt$static,
      replacement = opt$site
    ),
    overwrite = TRUE
  )
}
