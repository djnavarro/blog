
# targets functions -------------------------------------------------------

# basic options for the blog
get_options <- function() {
  list(
    root = rprojroot::find_root(rprojroot::has_file("_targets.R")),
    post = "post",
    site = "_site",
    static = "static"
  )
}

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


# user tools --------------------------------------------------------------

# this could be done better
clean_up <- function(opt = get_options()) {
  if (fs::dir_exists(fs::path(opt$root, "_targets"))) {
    fs::dir_delete(fs::path(opt$root, "_targets"))
  }
  if (fs::dir_exists(fs::path(opt$root, opt$site))) {
    fs::dir_delete(fs::path(opt$root, opt$site))
  }
}

# deletes a post: needs a confirmation check
delete_post <- function(num, opt = get_options()) {
  num <- stringr::str_pad(num, width = 3, pad = "0")
  fs::dir_delete(fs::path(opt$root, opt$post, num))
  fs::dir_delete(fs::path(opt$root, opt$site, num))
  order_posts()
}

# renumbers posts when one is deleted
order_posts <- function(opt = get_options()) {
  old <- fs::path(opt$root, opt$post) |>
    fs::dir_ls(type = "directory") |>
    unclass() |>
    unname()
  new <- fs::path(
    opt$root,
    opt$post,
    stringr::str_pad(seq_along(old), width = 3, pad = "0")
  ) |>
    unclass() |>
    unname()
  ind <- new != old
  old <- old[ind]
  new <- new[ind]
  purrr::walk2(old, new, \(o, n) {
    fs::dir_copy(path = o, new_path = n)
    fs::dir_delete(path = o)
  })
}

# create a new post
new_post <- function(slug, opt = get_options()) {
  slug <- stringr::str_to_lower(slug) |>
    stringr::str_replace_all("\\s", "-")
  num <- fs::path(opt$root, opt$post) |>
    fs::dir_ls(type = "directory") |>
    fs::path_file() |>
    as.numeric() |>
    max()
  num <- num + 1
  dir <- fs::path(
    opt$root,
    opt$post,
    stringr::str_pad(num, width = 3, pad = "0"),
    slug
  )
  out <- fs::path(dir, "index.rmd")
  rmd <- c(
    "---",
    paste("title: ", slug),
    "subtitle: subtitle-text",
    "---",
    "",
    "Text",
    "",
    "<br><br>",
    "",
    "[←](/)",
    ""
  )
  fs::dir_create(dir)
  brio::write_lines(rmd, out)
}


