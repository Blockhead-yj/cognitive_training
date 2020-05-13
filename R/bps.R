#' Calculates index scores for Choice Reaction Time game
#'
#' Now we majorly consider two indices: mean reaction time and count of correct
#' responses.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{bps_score}{BPS score.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
bps <- function(data, ...) {
  if (!all(utils::hasName(data, c("Phase", "Resp", "Type", "RT", "ACC")))) {
    warning("`Phase`, `Resp`, `Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        bps_score = NA_real_,
        is_normal = FALSE
      )
    )
  }
  bps_score <- data %>%
    dplyr::filter(.data$Phase == "test") %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(p_sim = sum(.data$Resp == "Similar") / dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "p_sim") %>%
    dplyr::transmute(bps_score = .data$lure - .data$foil)
  is_normal <- data %>%
    dplyr::filter(.data$Phase == "test") %>%
    dplyr::mutate(ACC_r = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L)) %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$ACC_r == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(bps_score, is_normal)
}
