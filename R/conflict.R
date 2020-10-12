#' Calculates index scores for games related to conflict effect
#'
#' Count of correct responses and congruency effect of correct response and
#' reaction time are all included. After `dataprocr2` version 0.0.4 (included),
#' this function will not work and [congeff()] works instead.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{nc}{Count of correct responses.}
#'   \item{mrt_inc}{Mean reaction time for incogruent trials.}
#'   \item{mrt_con}{Mean reaction time for congruent trials.}
#'   \item{cong_eff_rt}{Congruency effect of reaction time (RT), i.e., RT
#'     incongruency - RT congruency.}
#'   \item{pc_inc}{Percent of correct for incogruent trials.}
#'   \item{pc_con}{Percent of correct for congruent trials.}
#'   \item{cong_eff_pc}{Congruency effect of percent of correct (PC), i.e., PC
#'     congruency - PC incongruency.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
conflict <- function(data, ...) {
  .Defunct("congeff")
  if (!all(utils::hasName(data, c("Type", "ACC", "RT")))) {
    warning("`Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        nc = NA_real_,
        mrt_inc = NA_real_,
        mrt_con = NA_real_,
        cong_eff_rt = NA_real_,
        pc_inc = NA_real_,
        pc_con = NA_real_,
        cong_eff_pc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L))
  nc <- data_adj %>%
    dplyr::summarise(nc = sum(.data$acc_adj == 1))
  cong_eff <- data_adj %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(
      mrt = mean(.data$RT[.data$acc_adj == 1]),
      pc = mean(.data$acc_adj == 1)
    ) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = c("mrt", "pc")) %>%
    dplyr::transmute(
      mrt_inc = .data$mrt_Incongruent,
      mrt_con = .data$mrt_Congruent,
      cong_eff_rt = .data$mrt_Incongruent - .data$mrt_Congruent,
      pc_inc = .data$pc_Incongruent,
      pc_con = .data$pc_Congruent,
      cong_eff_pc = .data$pc_Congruent - .data$pc_Incongruent
    )
  is_normal <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(nc, cong_eff, is_normal)
}
