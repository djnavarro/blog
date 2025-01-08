# spell dice plot ---------------------------------------------------------

dice_data <- function(spells) {
  dice_dat <- spells |>
    select(name, level, description) |>
    mutate(
      dice_txt = str_extract_all(description, "\\b\\d+d\\d+\\b"),
      dice_txt = purrr::map(dice_txt, unique)
    ) |>
    unnest_longer(
      col = "dice_txt",
      values_to = "dice_txt",
      indices_to = "position"
    ) |>
    mutate(
      dice_num = dice_txt |> str_extract("\\d+(?=d)") |> as.numeric(),
      dice_die = dice_txt |> str_extract("(?<=d)\\d+") |> as.numeric(),
      dice_val = dice_num * (dice_die + 1)/2,
      dice_txt = factor(dice_txt) |> fct_reorder(dice_val)
    )
  return(dice_dat)
}

dice_plot <- function(dice_dat) {

  palette <- hcl.colors(n = 10, palette = "PuOr")

  labs <- dice_dat |>
    summarise(
      dice_txt = first(dice_txt),
      count = n(),
      .by = dice_txt
    )

  pic <- ggplot(
    data = dice_dat,
    mapping = aes(
      x = dice_txt,
      fill = factor(level)
    )
  ) +
    geom_bar(color = "#222") +
    geom_label_repel(
      data = labs,
      mapping = aes(
        x = dice_txt,
        y = count,
        label = dice_txt
      ),
      size = 3,
      direction = "y",
      seed = 1,
      nudge_y = 4,
      color = "#ccc",
      fill = "#222",
      arrow = NULL,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name = "Spell level",
      values = palette
    ) +
    scale_x_discrete(
      name = "Increasing average outcome \u27a1",
      breaks = NULL,
      expand = expansion(.05)
    ) +
    scale_y_continuous(name = NULL) +
    labs(title = "Dice rolls described in D&D spell descriptions") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#222"),
      text = element_text(color = "#ccc"),
      axis.text = element_text(color = "#ccc"),
      axis.title = element_text(color = "#ccc"),
      plot.margin = unit(c(1, 1, 1, 1), units = "cm"),
      legend.position = "inside",
      legend.position.inside = c(.3, .825),
      legend.direction = "horizontal",
      legend.title.position = "top",
      legend.byrow = TRUE
    )

  ggsave(
    filename = "dice_pic.png",
    plot = pic,
    width = 2000,
    height = 1000,
    units = "px",
    dpi = 150
  )

  return("dice_pic.png")
}


# schools of magic plot ---------------------------------------------------

# constructs the data frame used by geom_tile() later
scholastic_data <- function(spells) {
  spells |>
    select(name, school, bard:wizard) |>
    pivot_longer(
      cols = bard:wizard,
      names_to = "class",
      values_to = "castable"
    ) |>
    summarise(
      count = sum(castable),
      .by = c("school", "class")
    ) |>
    mutate(
      school = str_to_title(school),
      class  = str_to_title(class)
    )
}

# hierarchical clustering for the schools and classes
scholastic_clusters <- function(dat) {

  # matrix of counts for each school/class combination
  mat <- dat |>
    pivot_wider(
      names_from = "school",
      values_from = "count"
    ) |>
    as.data.frame()
  rownames(mat) <- mat$class
  mat$class <- NULL
  as.matrix(mat)

  # each school is a distribution over classes,
  # each class is a distribution over schools
  class_distribution  <- mat / replicate(ncol(mat), rowSums(mat))
  school_distribution <- t(mat) / (replicate(nrow(mat), colSums(mat)))

  # pairwise distances
  class_dissimilarity  <- dist(class_distribution)
  school_dissimilarity <- dist(school_distribution)

  # hierarchical clustering
  clusters <- list(
    class = hclust(class_dissimilarity, method = "average"),
    school = hclust(school_dissimilarity, method = "average")
  )

  return(clusters)
}

scholastic_plot <- function(dat, clusters) {

  pic <- ggplot(dat, aes(school, class, fill = count)) +
    geom_tile() +
    scale_x_dendro(
      clust = clusters$school,
      guide = guide_axis_dendro(n.dodge = 2),
      expand = expansion(0, 0),
      position = "top"
    ) +
    scale_y_dendro(
      clust = clusters$class,
      expand = expansion(0, 0)
    ) +
    scale_fill_distiller(palette = "RdPu") +
    labs(
      x = "The Schools of Magic",
      y = "The Classes of Character",
      fill = "Number of Learnable Spells"
    ) +
    coord_equal() +
    theme(
      plot.background = element_rect(fill = "#222", color = "#222"),
      plot.margin = unit(c(2, 2, 2, 2), units = "cm"),
      text = element_text(color = "#ccc"),
      axis.text = element_text(color = "#ccc"),
      axis.title = element_text(color = "#ccc"),
      axis.ticks = element_line(color = "#ccc"),
      legend.position = "bottom",
      legend.background = element_rect(fill = "#222", color = "#222")
    )

  ggsave(
    filename = "scholastic_pic.png",
    plot = pic,
    width = 1000,
    height = 1000,
    units = "px",
    dpi = 150
  )

  return("scholastic_pic.png")
}
