
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


  quantitative <- .data %>%
    select(where(is.numeric)) %>%
    {
      if (ncol(.) == 0) {
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
            names_to = c("Variable", ".value"),
            # names_vary = "slowest"
          )
      }
    }


  categorical <- .data %>%
    select(where(is.character), where(is.factor)) %>%
    {
      if (ncol(.) == 0) {
        NULL
      } else {
        (.) %>%
          summarize(across(
            .cols = everything(),
            ~ list(janitor::tabyl(.) %>% rename(Category = 1))
          )) %>%
          pivot_longer(
            cols = c(everything(), -any_of(group_vars(.data))),
            names_to = "Variable",
            values_to = "value"
          ) %>%
          unnest(c(value))
      }
    }


  return(list(

    quantitative = quantitative,

    categorical = categorical

  ))

}
