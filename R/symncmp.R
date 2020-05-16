#' Calculates index scores for Symbolic Number Comparison game.
#'
#' Several values including percentage of correct responses (pc), mean reaction
#' time (mrt), distance effect (dist_effect) and adjusted distance effect
#' (dist_effect_adj).
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{pc}{Percentage of correct responses.}
#'   \item{mrt}{Mean reaction time.}
#'   \item{dist_effect}{Distance effect.}
#'   \item{dist_effect_adj}{Adjusted distance effect.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
symncmp <- function(data, ...) {
  if (!all(utils::hasName(data, c("Small", "Big", "RT", "ACC")))) {
    warning("`Small`, `Big`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        mrt = NA_real_,
        dist_effect = NA_real_,
        dist_effect_adj = NA_real_,
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
  data_dist_effect <- data_adj %>%
    dplyr::filter(.data$acc_adj == 1) %>%
    dplyr::mutate(dist = .data$Big - .data$Small)
  dist_effect_orig <- stats::lm(RT ~ dist, data_dist_effect) %>%
    stats::coef() %>%
    `[`("dist")
  dist_effect <- data.frame(
    dist_effect = dist_effect_orig,
    dist_effect_adj = dist_effect_orig / basic$mrt
  )
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(basic, dist_effect, is_normal)
}
