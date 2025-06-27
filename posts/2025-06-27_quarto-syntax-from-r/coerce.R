as_quarto_tabset <- function(x, .level = 3L) {
  do.call(quarto_tabset, x)
}

as_quarto_section <- function(x, .level = 3L) {
  quarto_section(as.character(x), .level)
}
