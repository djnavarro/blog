
quarto_section <- function(.title, .level) {
  structure(
    rlang::list2(
      title = .title,
      level = .level
    ),
    class = "quarto_section"
  )
}

quarto_tabset <- function(..., .level = 3L) {
  tabs <- rlang::list2(...)
  structure(
    rlang::list2(
      content = tabs,
      title = names(tabs),
      level = .level
    ),
    class = "quarto_tabset"
  )
}

quarto_tabsec <- function(.content, .title, .level = 3L) {
  structure(
    rlang::list2(
      content = .content,
      title = .title,
      subtitle = names(.content),
      level = .level
    ),
    class = "quarto_tabsec"
  )
}

quarto_collection <- function(...) {
  structure(
    rlang::list2(
      collection = rlang::list2(...)
    ),
    class = "quarto_collection"
  )
}

