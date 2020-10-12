#' Calculates index scores for Symbolic Number Comparison game.
#'
#' Several values including percentage of correct responses (pc), mean reaction
#' time (mrt), distance effect (dist_effect) and adjusted distance effect
#' (dist_effect_adj).
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{pc}{Percentage of correct responses.}
#'   \item{mrt}{Mean reaction time.}
#'   \item{dist_eff}{Distance effect.}
#'   \item{dist_eff_adj}{Adjusted distance effect.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
symncmp <- function(data, ...) {
  if (!all(utils::hasName(data, c("Small", "Big", "RT", "ACC")))) {
    warning("`Small`, `Big`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        mrt = NA_real_,
        dist_eff = NA_real_,
        dist_eff_adj = NA_real_,
        is_normal = FALSE
      )
    )
  }
  # set as wrong for trials responding too quickly
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT <= 100, 0L, .data$ACC))
  basic <- data_adj %>%
    dplyr::summarise(
      pc = mean(.data$acc_adj == 1),
      mrt = mean(.data$RT[.data$acc_adj == 1])
    )
  data_dist_eff <- data_adj %>%
    dplyr::filter(.data$acc_adj == 1) %>%
    dplyr::mutate(dist = .data$Big - .data$Small)
  dist_eff_orig <- stats::lm(RT ~ dist, data_dist_eff) %>%
    stats::coef() %>%
    `[`("dist")
  dist_eff <- data.frame(
    dist_eff = dist_eff_orig,
    dist_eff_adj = dist_eff_orig / basic$mrt
  )
  is_normal <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(basic, dist_eff, is_normal)
}
