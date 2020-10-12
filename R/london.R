#' Calculates index scores for London Tower game.
#'
#' Two indices are returned: total scores (defined in the game itself), ratio
#' score (defined as the mean of the ratios between best steps and actual steps)
#' and mean level.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{tscore}{Total score defined by the game itself.}
#'   \item{ratio_score}{Mean of the ratio between best steps and actual steps.}
#'   \item{mean_level}{Mean level reached.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
london <- function(data, ...) {
  if (!all(utils::hasName(data, c("LeveL", "Score", "Outcome")))) {
    warning("`LeveL`, `Score` and `Outcome` variables are required.")
    return(
      data.frame(
        tscore = NA_real_,
        ratio_score = NA_real_,
        mean_level = NA_real_,
        is_normal = FALSE
      )
    )
  }
  tscore <- data %>%
    dplyr::summarise(tscore = sum(.data$Score))
  ratio_score <- data %>%
    dplyr::mutate(
      ratio = dplyr::if_else(
        .data$Finished == 0,
        0, .data$LeveL / .data$StepsUsed
      )
    ) %>%
    dplyr::summarise(ratio_score = mean(.data$ratio))
  mean_level <- data %>%
    dplyr::group_by(.data$LeveL) %>%
    dplyr::summarise(pc = mean(.data$Outcome == 1)) %>%
    dplyr::summarise(mean_level = min(.data$LeveL) - 0.5 + sum(.data$pc))
  cbind(tscore, ratio_score, mean_level, is_normal = TRUE)
}
