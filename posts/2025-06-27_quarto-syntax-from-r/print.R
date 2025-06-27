
knit_print.quarto_section <- function(x, ...) {
  hashes <- paste(rep("#", x$level), collapse = "")
  header <- paste0("\n\n", hashes, " ", x$title, "\n\n")
  cat(header)
}

knit_print.quarto_tabset <- function(x, ...) {

  # open tabset
  cat("\n\n::: {.panel-tabset}\n\n")
  for(i in seq_along(x$content)) {

    # create tab with section header
    hashes <- paste(rep("#", x$level), collapse = "")
    header <- paste0("\n\n", hashes, " ", x$title[i], "\n\n")
    cat(header)

    # output
    if (ggplot2::is_ggplot(x$content[[i]])) {
      knitr::knit_print(x$content[[i]])
    } else {
      cat("<pre>")
      knitr::knit_print(x$content[[i]])
      cat("</pre>")
    }

  }

  # close tabset
  cat("\n\n::: \n\n")
}

knit_print.quarto_tabsec <- function(x, ...) {
  knitr::knit_print(quarto_section(.title = x$title, .level = x$level))
  knitr::knit_print(quarto_tabset(!!!x$content, .level = x$level + 1))
}

knit_print.quarto_collection <- function(x, ...) {
  purrr::walk(x$collection, knitr::knit_print)
} 


# regular print method is the same as knit_print
print.quarto_section <- knit_print.quarto_section
print.quarto_tabset <- knit_print.quarto_tabset
print.quarto_tabsec <- knit_print.quarto_tabsec
print.quarto_collection <- knit_print.quarto_collection
