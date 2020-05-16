#' Calculates index scores for games related to conflict effect
#'
#' Count of correct responses and congruency effect of correct response and
#' reaction time are all included.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{count_correct}{Count of correct responses.}
#'   \item{cong_eff_rt}{Congruency effect of reaction time (RT), i.e., RT
#'     incongruency - RT congruency.}
#'   \item{cong_eff_pc}{Congruency effect of percent of correct (PC), i.e., PC
#'     congruency - PC incongruency.}
#'   \item{is_normal}{Checking result whether the data is normal.} }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
conflict <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "ACC", "RT")))) {
    warning("`Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        count_correct = NA_real_,
        cong_eff_rt = NA_real_,
        cong_eff_pc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L))
  count_correct <- data_adj %>%
    dplyr::summarise(count_correct = sum(.data$acc_adj == 1))
  cong_eff <- data_adj %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(
      mrt = mean(.data$RT[.data$acc_adj == 1]),
      pc = mean(.data$acc_adj == 1)
    ) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = c("mrt", "pc")) %>%
    dplyr::transmute(
      cong_eff_rt = .data$mrt_Incongruent - .data$mrt_Congruent,
      cong_eff_pc = .data$pc_Congruent - .data$pc_Incongruent
    )
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(count_correct, cong_eff, is_normal)
}
