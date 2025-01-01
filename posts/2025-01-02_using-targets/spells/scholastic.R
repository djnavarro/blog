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

scholastic_plot <- function(dat, clusters, output) {

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

  out <- path(output, "scholastic_pic.png")

  ggsave(
    filename = out,
    plot = pic,
    width = 1000,
    height = 1000,
    units = "px",
    dpi = 150
  )

  return(out)
}
