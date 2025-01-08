# load packages
library(tibble)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(ggrepel)
library(legendry)

# read analysis script
source("analysis.R")

# setup
input  <- "spells.csv"
spells <- read_csv(input, show_col_types = FALSE)

# make the spell dice plot & write to output
dice_dat <- dice_data(spells)
dice_pic <- dice_plot(dice_dat)

# make the schools of magic plot & write to output
scholastic_dat  <- scholastic_data(spells)
scholastic_clus <- scholastic_clusters(scholastic_dat)
scholastic_pic  <- scholastic_plot(scholastic_dat, scholastic_clus)
