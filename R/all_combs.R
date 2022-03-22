
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
all_combs <- function(x) {

  map(seq_along(x), ~ combn(x, .x, simplify = FALSE)) %>%
    flatten()

}
