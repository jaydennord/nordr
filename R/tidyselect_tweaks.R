#' @importFrom dplyr rename_with
#' @importFrom dplyr select

#' @export
list_rename_with <- function(.list, .fn, ...) {

  .fn <- rlang::as_function(.fn)
  names <- .fn(names(.list), ...)

  rlang::set_names(.list, names)

}

#' @export
list_select <- function(.list, ...) {

  pos <- tidyselect::eval_select(
    rlang::expr(c(...)),
    .list
  )

  purrr::set_names(.list[pos], names(pos))

}

#' @export
char_select <- function(.x, ...) {

  pos <- tidyselect::eval_select(
    rlang::expr(c(...)),
    purrr::set_names(.x)
  )

  .x[pos]

}
