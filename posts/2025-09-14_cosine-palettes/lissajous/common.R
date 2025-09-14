# Use this file for simple helper functions that aren't really part of the
# system itself. I usually only include functions that help me manage the file
# names and paths for output images. Note that this takes a dependency on the
# here package. By design it does *not* take a dependency on arttools

# Check that the here package exists!
if (!require("here", quietly = TRUE)) {
  message("common.R uses the 'here' package: you may need to install it")
}

# Function to left-pad an integer with leading zeros to ensure that output
# strings all have the same length, which in turn means that the image files
# will sort in the correct order later on when you put them on the web. It's
# essentially a hacky version of something like stringr::str_pad(), so if you're
# okay with the system taking stringr as a dependency you can ditch this
# function entirely
tidy_int_string <- function(x, width) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(x)
  x_num <- suppressWarnings(as.numeric(x))
  if (is.na(x_num)) return(x)
  x_int <- suppressWarnings(as.integer(x))
  if (x_int != x_num) return(x)
  x_chr <- as.character(x_int)
  len <- nchar(x_chr)
  if (len > width) {
    warning("integer", x, "is wider than expected width", width, "cannot pad")
  }
  if (len == width) return(x_chr)
  paste0(c(rep("0", width - len), x_chr), collapse = "")
}

# Function to replace white space, period, or underscores with hyphens. This
# helps keep output file names clean (i.e., white spaces are bad, periods should
# ideally only be used to begin a file extension), and it also ensures that
# hyphens separate words within the same part of the file name whereas
# underscores are used to distinguish the different parts of the file name
tidy_name_string<- function(x, replace = "-") {
  pattern <- "[[:space:]._]+" # white space, period, or underscore
  gsub(pattern, replace, x)
}

# Output file name:
#
#   [name]_[version]_[id].[format]
#
output_file <- function(name, version, id, format) {
  # zero pad version and id if necessary
  version <- tidy_int_string(version, width = 2)
  id <- tidy_int_string(id, width = 4)

  # coerce to character, probably unnecessarily
  name <- as.character(name)
  version <- as.character(version)
  id <- as.character(id)
  format <- as.character(format)

  # replace white space, dots, or underscores with hyphens
  name <- tidy_name_string(name)
  version <- tidy_name_string(version)
  id <- tidy_name_string(id)
  format <- tidy_name_string(format, replace = "")

  # paste the parts of the name together, separating with underscore
  file_name <- paste(name, version, id, sep = "_")

  # append file extension, ensure lower case, and return
  file_name <- paste(file_name, format, sep = ".")
  file_name <- tolower(file_name)
  file_name
}

# Output file path:
#
#   [repo-folder]/output/[version]/[name]_[version]_[id].[format]
#
output_path <- function(name,
                        version,
                        id,
                        format,
                        output_dir = here::here("output")) {
  # construct the file name
  file_name <- output_file(name, version, id, format)

  # enforce constraints on version
  version <- tidy_int_string(version, width = 2)
  version <- as.character(version)
  version <- tidy_name_string(version)
  version <- tolower(version)

  # specify the path to the output folder, creating it if necessary
  output_dir <- file.path(output_dir, version)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # return fully qualified path
  file.path(output_dir, file_name)
}

# An infuriatingly common mistake I make when iterating on my generative art
# systems is updating a file name, or a version number, but not both. This can
# lead to frustration when the incorrectly-modified version of the system
# writes images to the wrong place and with the wrong names, and sometimes
# overwrites images output previously. The check_version() function scans the
# source folder looking for all files that start with a particular system name,
# compares the version string within the file to the one implicit in the file
# name to ensure consistency
system_versions <- function(name, source_dir = here::here("source")) {
  source_files <- list.files(source_dir, pattern = name)
  source_files
  filename_version <- vapply(
    strsplit(source_files, "[_.]"),
    function(x) gsub("^0+", "", as.character(x[2])),
    character(1)
  )
  source_version <- vapply(
    source_files,
    function(x) {
      src <- readLines(file.path(source_dir, x))
      ver <- grep("^version", src, value = TRUE)[1]
      ver <- parse(text = ver) # returns expression object
      ver <- ver[[1]][[3]] # extract RHS from expression
      gsub("^0+", "", as.character(ver))
    },
    character(1)
  )

  # note that the versions returned are always character type
  data.frame(
    file = source_files,
    filename_version = filename_version,
    source_version = source_version,
    row.names = NULL
  )
}

assert_version_consistency <- function(name) {
  versions <- system_versions(name)
  if (
    anyDuplicated(versions$filename_version) ||
    anyDuplicated(versions$source_version) ||
    any(versions$filename_version != versions$source_version)
  ) {
    msg <- paste0(
      "versions are inconsistent, call 'system_versions(\"",
      name,
      "\")' for details"
    )
    stop(msg, call. = FALSE)
  }
  invisible(versions)
}
