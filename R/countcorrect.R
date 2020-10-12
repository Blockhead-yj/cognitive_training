#' Calculates index scores for games only considering correct response numbers.
#'
#' This is just to find out the count of correct responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{nc}{Count of correct responses.}
#'   \item{pc}{Percent of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
countcorrect <- function(data, ...) {
  acc_vars <- c("ACC", "Repetition", "Correctness")
  acc_var_idx <- utils::hasName(data, acc_vars)
  if (sum(acc_var_idx) != 1) {
    warning("One and only one accuracy related variable, i.e., `ACC`, `Repetition` or `Correctness`, is required.")
    return(
      data.frame(nc = NA, pc = NA, is_normal = FALSE)
    )
  }
  acc_var <- acc_vars[acc_var_idx]
  if (utils::hasName(data, "RT")) {
    data_adj <- data %>%
      dplyr::mutate(
        acc_adj = dplyr::if_else(.data$RT >= 100, .data[[acc_var]], 0L)
      )
  } else {
    data_adj <- data %>%
      dplyr::mutate(acc_adj = .data[[acc_var]])
  }
  if (is.character(data_adj$acc_adj)) {
    data_adj <- data_adj %>%
      dplyr::filter(!is.na(.data$acc_adj) & .data$acc_adj != "NULL") %>%
      dplyr::summarise(
        acc_adj = .data$acc_adj %>%
          stringr::str_c(collapse = "-") %>%
          stringr::str_split("-", simplify = TRUE) %>%
          as.numeric() %>%
          list()
      ) %>%
      tidyr::unnest(.data$acc_adj)
  }
  data_adj %>%
    dplyr::summarise(
      nc = sum(.data$acc_adj == 1),
      pc = mean(.data$acc_adj == 1),
      is_normal = TRUE
    )
}
