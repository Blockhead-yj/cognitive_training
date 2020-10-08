#' Calculates index scores for games only considering correct response numbers.
#'
#' This is just to find out the count of correct responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{nc}{Count of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @importFrom rlang .data
#' @importFrom rlang !!
#' @export
countcorrect <- function(data, ...) {
  acc_vars <- c("ACC", "Repetition")
  acc_var_idx <- utils::hasName(data, acc_vars)
  if (!any(acc_var_idx)) {
    warning("Accuracy related variable, i.e., `ACC` or `Repetition`, is required.")
    return(
      data.frame(nc = NA_real_, is_normal = FALSE)
    )
  }
  acc_var <- acc_vars[acc_var_idx]
  if (utils::hasName(data, "RT")) {
    data_adj <- data %>%
      dplyr::mutate(
        acc_adj = dplyr::if_else(.data$RT >= 100, !!rlang::sym(acc_var), 0L)
      )
  } else {
    data_adj <- data %>%
      dplyr::mutate(acc_adj = !!rlang::sym(acc_var))
  }
  data_adj %>%
    dplyr::summarise(nc = sum(.data$acc_adj == 1), is_normal = TRUE)
}
