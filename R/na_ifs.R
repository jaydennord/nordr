#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
na_ifs <- function(x, ...) {

  x[x %in% c(...)]
  x

}
