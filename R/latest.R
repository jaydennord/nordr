

#' Get path of latest matched file
#'
#' @description
#' This function searches for the latest file that matches a search pattern in
#' a given directory. This is particular useful for exported datasets that
#' include ever changing download times in their filename.
#'
#'
#' @param pattern Regular expression pattern
#' @param path A character vector of full path names; the default corresponds
#'    to the working directory, [getwd()]. Tilde expansion (see [path.expand]) is
#'    performed. Missing values will be ignored. Elements with a marked encoding
#'    will be converted to the native encoding (and if that fails, considered
#'    non-existent).
#' @param ... Additional parameters passed to `dir`.
#'
#' @returns A character vector of length 1 that contains the most recent file
#'    that matches `pattern`.
#'
#'    An error is thrown if the `pattern` cannot be matched. A confirmation
#'    message is printed if the `pattern` is found.
#' @export
#'
latest <- function(pattern, path = ".", ...) {

  files <- dir(path, pattern, ...)

  if (length(files) == 0) stop("Pattern did not match.")

  mtimes <- file.info(files, extra_cols = FALSE)$mtime
  out <- files[which(mtimes == max(mtimes, na.rm = TRUE))]

  message("Choosing file: ", out)

  out

}
