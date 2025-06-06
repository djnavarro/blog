library(targets)
tar_source("_liteblog.R")

list(

  # define blog configuration
  tar_target(
    name = blog,
    command = Liteblog$new(
      root = rprojroot::find_root(
        rprojroot::has_file("_liteblog.R")
      ),
      source = "source",
      output = "site",
      url = "liteblog.djnavarro.net"
    )
  ),

  # track configuration files
  tar_target(
    name = blog_rds,
    command = saveRDS(blog, file = "_liteblog.rds"),
    format = "file"
  ),
  tar_target(blog_css, "_liteblog.css", format = "file"),
  tar_target(blog_hdr, "_liteblog-header.html", format = "file"),
  tar_target(blog_ftr, "_liteblog-footer.html", format = "file"),

  # detect file paths (always run)
  tar_target(
    name = post_paths,
    command = blog$find_posts(),
    cue = tar_cue("always")
  ),
  tar_target(
    name = static_paths,
    command = blog$find_static(),
    cue = tar_cue("always")
  ),

  # specify file targets
  tar_target(
    name = post_files,
    command = post_paths,
    pattern = map(post_paths),
    format = "file"
  ),
  tar_target(
    name = static_files,
    command = static_paths,
    pattern = map(static_paths),
    format = "file"
  ),

  # fuse targets depend on blog configuration files
  # copy targets don't need dependencies
  tar_target(
    name = post_fuse,
    command = blog$fuse_post(
      post_files,
      blog_css,
      blog_hdr,
      blog_ftr
    ),
    pattern = map(post_files)
  ),
  tar_target(
    name = static_copy,
    command = blog$copy_static(static_files),
    pattern = map(static_files)
  )
)
