#' Calculates index scores for games related to congruency effect
#'
#' Count of correct responses and congruency effect of correct response and
#' reaction time are all included.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mrt_inc}{Mean reaction time for incogruent trials.}
#'   \item{mrt_con}{Mean reaction time for congruent trials.}
#'   \item{cong_eff_rt}{Congruency effect of reaction time (RT), i.e., RT
#'     incongruency - RT congruency.}
#'   \item{pc_inc}{Percent of correct for incogruent trials.}
#'   \item{pc_con}{Percent of correct for congruent trials.}
#'   \item{cong_eff_pc}{Congruency effect of percent of correct (PC), i.e., PC
#'     congruency - PC incongruency.}
#'   \item{nc}{Count of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
congeff <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "ACC", "RT")))) {
    warning("`Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        mrt_inc = NA,
        mrt_con = NA,
        cong_eff_rt = NA,
        pc_inc = NA,
        pc_con = NA,
        cong_eff_pc = NA,
        nc = NA,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L))
  cong_eff <- calc_cong_eff(data_adj, name_acc = "acc_adj")
  nc_and_validation <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(
      .data$nc,
      is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5)
    )
  cbind(cong_eff, nc_and_validation)
}
