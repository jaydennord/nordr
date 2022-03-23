
#' Title
#' @description
#' Based on \code{NCmisc::list.functions.in.file}
#' @return
#' @export
#'
#' @examples
#'
list_funs_pkgs <- function() {


  x <- rstudioapi::getSourceEditorContext()
  tmp <- getParseData(parse(text = x$contents, keep.source = TRUE))
  nms <- tmp$text[tmp$token == "SYMBOL_FUNCTION_CALL"]
  funs <- unique(nms)

  pkgs <- lapply(funs, function(fun) {
    find(fun, mode = "function") -> .
    gsub("package:", "", .)
  })

  # unique(unlist(pkgs))
  pkgs_collapsed <- paste(pkgs)

  tapply(funs, pkgs_collapsed, c)

}
