#' Calculates index scores for games only considering correct response numbers.
#'
#' This is just to find out the count of correct responses.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{count_correct}{Count of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang !!
#' @export
count <- function(data, ...) {
  acc_vars <- c("ACC", "Repetition")
  acc_var_idx <- utils::hasName(data, acc_vars)
  if (!any(acc_var_idx)) {
    warning("Accuracy related variable, i.e., `ACC` or `Repetition`, is required.")
    return(
      data.frame(count_correct = NA_real_, is_normal = FALSE)
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
    dplyr::summarise(count_correct = sum(.data$acc_adj == 1), is_normal = TRUE)
}
