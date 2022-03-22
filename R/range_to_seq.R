#' Title
#'
#' @param x
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
range_to_seq <- function (x, sep = "-") {

  # ranged <- str_detect(x, sep) & !is.na(x)
  ranged <- grepl(sep, x, perl = TRUE)

  strsplit(x, sep) %>%
    map_if(ranged, ~ seq(.[1], .[2])) %>%
    map_if(!ranged, as.numeric)

}
