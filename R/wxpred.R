#' Calculates index for Weather Prediction Paradigm game
#'
#' Percent of correct for each block is returned.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{pc_b1}{Percent of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
wxpred <- function(data, ...) {
  if (!all(utils::hasName(data, c("Block", "ACC", "RT")))) {
    warning("`Block`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        pc_b1 = NA_real_,
        pc_b2 = NA_real_,
        pc_b3 = NA_real_,
        pc_b4 = NA_real_,
        is_normal = FALSE
      )
    )
  }
  if (max(data$Block) != 4) {
    warning("Number of blocks is not 4, will return NA values.")
    return(
      data.frame(
        pc_b1 = NA_real_,
        pc_b2 = NA_real_,
        pc_b3 = NA_real_,
        pc_b4 = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L))
  pc <- data_adj %>%
    dplyr::group_by(.data$Block) %>%
    dplyr::summarise(pc = mean(.data$acc_adj == 1)) %>%
    tidyr::pivot_wider(names_from = "Block", names_prefix = "pc_b", values_from = "pc")
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(pc, is_normal)
}