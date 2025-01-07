library(targets)

# specify required packages
tar_option_set(packages = c(
  "tibble", "readr", "ggplot2", "dplyr", "stringr",
  "tidyr", "forcats", "ggrepel", "legendry"
))

# read analysis script
tar_source("analysis.R")

list(
  # setup
  tar_target(input, "spells.csv", format = "file"),
  tar_target(spells, read_csv(input, show_col_types = FALSE)),

  # dice plot
  tar_target(dice_dat, dice_data(spells)),
  tar_target(dice_pic, dice_plot(dice_dat)),

  # scholastic plot
  tar_target(scholastic_dat, scholastic_data(spells)),
  tar_target(scholastic_clus, scholastic_clusters(scholastic_dat)),
  tar_target(
    scholastic_pic,
    scholastic_plot(scholastic_dat, scholastic_clus)
  )
)
