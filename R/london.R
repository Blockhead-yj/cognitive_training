#' Calculates index scores for London Tower game.
#'
#' Two indices are returned: total scores (defined in the game itself) and mean
#' level.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{tscore}{Total score defined by the game itself.}
#'   \item{mean_level}{Mean level reached.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
london <- function(data, ...) {
  if (!all(utils::hasName(data, c("LeveL", "Score", "Outcome")))) {
    warning("`LeveL`, `Score` and `Outcome` variables are required.")
    return(
      data.frame(
        tscore = NA_real_,
        mean_level = NA_real_,
        is_normal = FALSE
      )
    )
  }
  tscore <- data %>%
    dplyr::summarise(tscore = sum(.data$Score))
  mean_level <- data %>%
    dplyr::group_by(.data$LeveL) %>%
    dplyr::summarise(pc = mean(.data$Outcome == 1)) %>%
    dplyr::summarise(mean_level = min(.data$LeveL) - 0.5 + sum(.data$pc))
  cbind(tscore, mean_level, is_normal = TRUE)
}
