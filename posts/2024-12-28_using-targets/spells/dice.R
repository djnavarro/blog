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

dice_plot <- function(dice_dat, output) {

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
    labs(
      title = "Frequency of dice rolls described in D&D spell descriptions, by spell level",
      subtitle = "Or whatever",
      caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-12-17"
    ) +
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

  out <- path(output, "dice_pic.png")

  ggsave(
    filename = out,
    plot = pic,
    width = 2000,
    height = 1000,
    units = "px",
    dpi = 150
  )

  return(out)
}
