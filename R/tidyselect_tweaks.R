#' @importFrom dplyr rename_with
#' @importFrom dplyr select

#' @export
rename_with.list <- function(.data, .fn, ...) {

  .fn <- rlang::as_function(.fn)
  names <- .fn(names(.data), ...)

  rlang::set_names(.data, names)

}

#' @export
select.list <- function(.data, ...) {

  pos <- tidyselect::eval_select(expr(c(...)), .data)
  rlang::set_names(.data[pos], names(pos))

}
