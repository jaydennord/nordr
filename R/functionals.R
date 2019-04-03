#' @export
#' @title Make functions handle errors better
#' @description Does what it does.
#' @param fun
#' @examples

robustify <- function(fun) {
  fun <- match.fun(fun)

  function(...) {

    error <- warn <- NA_character_
    result <- withCallingHandlers(
      tryCatch(
        fun(...),
        error = function(cnd) {
          error <<- conditionMessage(cnd)
          NULL
        }
      ),
      warning = function(cnd) {
        warn <<- conditionMessage(cnd)
        invokeRestart("muffleWarning")
      }
    )

    list(
      result = result,
      warning = warn,
      error = error
    )

  }
}

#' @export
#' @title Repeat function until no errors
#' @description Dangerous...
#' @param fun
#' @examples


repeatedly <- function(fun) {
  fun <- match.fun(fun)

  function(...) {
    attempt <- 1
    result <- NULL

    while(is.null(result)) {
      result <- tryCatch(
        fun(...),
        error = function(cnd) {
          attempt <<- attempt + 1
          NULL
        }
      )

    }

    list(result = result, attempts = attempt)
  }
}
