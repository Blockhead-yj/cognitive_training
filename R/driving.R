#' Calculates index scores for Driving game.
#'
#' Caculcate the ratio of still duration in yellow light state.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{still_ratio}{The ratio of still duration in yellow light state.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
driving <- function(data, ...) {
  if (!all(utils::hasName(data, c("YellowDur", "StillDur", "StillLight")))) {
    warning("`YellowDur`, `StillDur` and `StillLight` variables are required.")
    return(
      data.frame(
        still_ratio = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data.frame(
    still_dur = data$StillDur %>%
      paste(collapse = "-") %>%
      strsplit("-") %>%
      unlist() %>%
      as.numeric(),
    still_light = data$StillLight %>%
      paste(collapse = "-") %>%
      strsplit("-") %>%
      unlist()
  ) %>%
    dplyr::summarise(
      still_ratio = sum(.data$still_dur[.data$still_light == "Yellow"]) /
        sum(data$YellowDur),
      is_normal = TRUE
    )
}
