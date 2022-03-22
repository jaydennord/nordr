
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

nan_to_na <- function(x) {

  x[is.nan(x)] <- NA_real_
  x

}
