.desc_quant_fns <- list(
  mean    = \(x) mean(x, na.rm = TRUE),
  sd      = \(x) sd(x, na.rm = TRUE),
  skew    = \(x) if (sum(!is.na(x)) < 3) NA_real_ else e1071::skewness(x, na.rm = TRUE, type = 2),
  exkurt  = \(x) if (sum(!is.na(x)) < 4) NA_real_ else e1071::kurtosis(x, na.rm = TRUE, type = 2),
  min     = \(x) min(x, na.rm = TRUE),
  max     = \(x) max(x, na.rm = TRUE),
  valid_n = \(x) sum(!is.na(x)),
  n       = \(x) length(x),
  quart1  = \(x) quantile(x, .25, na.rm = TRUE),
  median  = \(x) median(x, na.rm = TRUE),
  quart3  = \(x) quantile(x, .75, na.rm = TRUE)
)



#' Descriptive statistics and frequency tables
#'
#' @description
#' Convenient functions for describing data.
#'
#' * `desc_quant` runs quantitative descriptives of quantitative data.
#' * `desc_freq` runs frequency statistics for categorical data.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble)
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'    expressions separated by commas. Variable names can be used as if they
#'    were positions in the data frame, so expressions like `x:y` can
#'    be used to select a range of variables.
#' @param .by <[`tidy-select`][dplyr_tidy_select]> Optionally, a selection of
#'    columns to group by for just this operation.
#' @param .fns Named list of functions to apply to the columns specified in
#'    `...`. The names of the list will be the column names of the resulting
#'    data frame.
#'
#' @returns An object *usually* of the same type as `.data`.
#'
#'    Each column is the result of a function defined in `.fns`. Each
#'    row is a grouping variable defined in `.by`, if any.
#'
#' @export
#'
#' @examples # no examples available
#' @rdname desc_funs

desc_quant <- function(
    .data, ...,
    .by = NULL,
    .fns = .desc_quant_fns
) {

  .by <- rlang::enexpr(.by)
  grps <- if (is.null(.by)) character() else names(tidyselect::eval_select(.by, .data))

  .data |>
    dplyr::summarize(
      .by = dplyr::any_of(grps),
      dplyr::across(
        .cols = c(...),
        .fns  = .fns,
        .names = "{.col}<>{.fn}"
      )
    ) |>
    tidyr::pivot_longer(
      cols = -dplyr::any_of(grps),
      names_sep = "<>",
      names_to = c("Variable", ".value")
    )

}



#' @export
#' @rdname desc_funs
desc_freq <- function(.data, ..., .by = NULL) {

  grps <- colnames(.data)[tidyselect::eval_select(rlang::enexpr(.by) , .data)]
  vars <- colnames(.data)[tidyselect::eval_select(rlang::expr(c(...)), .data)] |>
    setdiff(grps) |>
    rlang::set_names()

  purrr::imap(vars, function(x, y) {

    .data |>
      dplyr::count(
        dplyr::pick(dplyr::any_of(grps)),
        Variable = y,
        Category = .data[[x]],
        .drop = FALSE
      ) |>
      dplyr::mutate(
        .by = dplyr::any_of(grps),
        per = n / sum(n),
        valid_per = ifelse(
          is.na(Category),
          NA_real_,
          n / sum(n[!is.na(Category)])
        )
      )

  }) |>
    dplyr::bind_rows()

}
