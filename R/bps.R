#' Calculates index scores for Behavioral Pattern Separation (BPS) game
#'
#' The index was developed by Stark et. al. (2013), named as "BPS score".
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{pc}{Percent of correct responses.}
#'   \item{p_sim_lure}{Percent of similar responses for "lure" stimuli.}
#'   \item{p_sim_foil}{Percent of similar responses for "foil" stimuli.}
#'   \item{p_sim_old}{Percent of similar responses for "target" (i.e., "old") stimuli.}
#'   \item{bps_score}{BPS score.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
bps <- function(data, ...) {
  if (!all(utils::hasName(data, c("Phase", "Resp", "Type", "RT", "ACC")))) {
    warning("`Phase`, `Resp`, `Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        p_sim_lure = NA_real_,
        p_sim_foil = NA_real_,
        p_sim_old = NA_real_,
        bps_score = NA_real_,
        is_normal = FALSE
      )
    )
  }
  pc_all <- data %>%
    dplyr::filter(.data$Phase == "test") %>%
    dplyr::summarise(pc = mean(.data$ACC == 1))
  bps_score <- data %>%
    dplyr::filter(.data$Phase == "test") %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(p_sim = sum(.data$Resp == "Similar") / dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "p_sim") %>%
    dplyr::transmute(
      p_sim_lure = .data$lure,
      p_sim_foil = .data$foil,
      p_sim_old = .data$target,
      bps_score = .data$lure - .data$foil
    )
  is_normal <- data %>%
    dplyr::filter(.data$Phase == "test") %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L)) %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(pc_all, bps_score, is_normal)
}
