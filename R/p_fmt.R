
#' Title
#'
#' @param p
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
#'
#'
p_fmt <- function(p, digits = 3) {

  sub("0\\.", ".", ifelse(
    p <= 10^-digits,
    sprintf(paste0("<%.", digits, "f"), 10^-digits),
    sprintf(paste0( "%.", digits, "f"), p)
  ))

}
