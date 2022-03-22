
#' @title
#' Make SPSS operations for a mutation
#'
#' @description
#' Making SPSS operations for a mutation
#' @param .table
#' A list or data frame with three elements:
#' \enumerate{
#' \item Character vector of variable names
#' \item Character vector of variable labels
#' \item Character vector of value labels, each element of the form \code{label1 = value1, label2 = value2, ...}
#' }
#' @param na_values
#' Values to be marked as \code{NA} in SPSS. Applied to all variables given by \code{.table}.
#' @return
#' A list of quosures to be spliced into a \code{mutate} function.
#' @export
#'
#' @examples
make_spss_ops <- function(.table, na_values = -99) {

  .table %>%
    mutate(

      across(1, set_names),
      across(2, map_if, is.na, ~ NULL),
      across(3, ~ {

        paste0("c(", ., ")") %>%
          map(~ eval(parse(text = .x))) %>%
          map_if(naniar::all_na, ~ NULL)

      })

    ) %>%
    pmap(~ quo({

      na <- switch(
        EXPR = typeof(!! sym(..1)),
        integer = !! as.integer(na_values),
        double = !! as.double(na_values),
        character = !! as.character(na_values),
        NULL
      )

      labelled_spss(
        !! sym(..1),
        label = !! ..2,
        labels = !! ..3,
        na_values = na
      )

    }))

}

