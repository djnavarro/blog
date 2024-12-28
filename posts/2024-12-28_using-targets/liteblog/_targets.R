library(targets)
tar_source(files = c("scripts/common.R", "scripts/build.R"))
list(
  tar_target(opt, get_options()),
  tar_target(page_list, page_paths(opt)),
  tar_target(page, page_list, pattern = map(page_list), format = "file"),
  tar_target(fuse, fuse_page(page), pattern = map(page)),
  tar_target(static, copy_static(opt))
)
