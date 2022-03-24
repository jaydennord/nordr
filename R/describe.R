
#' Title
#'
#' @param .data
#' @param ...
#' @param .quant_funs
#'
#' @return
#' @export
#'
#' @examples
#'

describe <- function(
  .data, ...,
  .quant_funs = list(
    M  = ~ mean(., na.rm = TRUE),
    SD = ~ sd(., na.rm = TRUE),
    N  = ~ sum(!is.na(.))
  )
) {

  grps <- group_vars(.data)

  quantitative <- .data %>%
    select(any_of(grps), where(is.numeric)) %>%
    {
      if (ncol(.) == length(grps)) {
        NULL
      } else {
        (.) %>%
          summarize(across(
            everything(),
            .quant_funs
          ), .groups = "keep") %>%
          pivot_longer(
            cols = c(everything(), -any_of(group_vars(.data))),
            names_pattern = paste0("^(.*)_(", paste(names(.quant_funs), collapse = "|"), ")"),
            names_to = c("Variable", ".value")
          )
      }
    }


  i <- imap_lgl(.data, ~ (! .y %in% grps) & any(class(.x) %in% c("factor", "character", "logical")))
  cat_cols <- names(.data)[i]

  if (length(cat_cols) == 0) {
    categorical <- NULL
  } else {
    categorical <- cat_cols %>%
      set_names() %>%
      map(~ {

        .data %>%
          count(!!! syms(grps), !! sym(.x), .drop = FALSE) %>%
          rename(Category = all_of(.x)) %>%
          mutate(
            perc = n / sum(n),
            valid_perc = ifelse(
              is.na(Category),
              NA_real_,
              n / sum(n[!is.na(Category)])
            )
          )


      })
  }

  return(list(

    quantitative = quantitative,

    categorical = categorical

  ))

}
