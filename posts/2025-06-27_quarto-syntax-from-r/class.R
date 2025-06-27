
# tabs and sections -------------------------------------------

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
  .content <- rlang::list2(...)
  structure(
    rlang::list2(
      content = .content,
      title = names(.content),
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

# groups of output -------------------------------------------

quarto_output <- function(...) {
  structure(
    rlang::list2(
      content = rlang::list2(...)
    ),
    class = "quarto_output"
  )
}

quarto_markdown <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = "quarto_markdown"
  )
}

quarto_paragraph <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = "quarto_paragraph"
  )
}

# divs and spans -------------------------------------------

# TODO: should this be a .content or dots?
quarto_div <- function(..., .class, .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      class = .class,
      sep = .sep,
    ),
    class = "quarto_div"
  )
}

quarto_span <- function(.content, .class = NULL) {
  structure(
    rlang::list2(
      content = .content,
      class = .class
    ),
    class = "quarto_span"
  )
}
