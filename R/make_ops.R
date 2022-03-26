
#' Title
#'
#' @param .data
#' @param names
#' @param op
#'
#' @return
#' @export
#'
#' @examples

make_ops <- function (.data, names, op) {

  names <- f_rhs(names)
  op <- f_rhs(op)

  set_names(
    x  = pmap(.data, ~ inject(quo(!! op))),
    nm = pmap_chr(.data, ~ eval(names))
  )

}

