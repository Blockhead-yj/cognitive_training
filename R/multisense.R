#' Calculates index scores for Multisense games.
#'
#' Three mean reaction times as to Image, Sound and Mixed stimuli are returned.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mrt_image}{Mean reaction time of Image stimuli.}
#'   \item{mrt_sound}{Mean reaction time of Sound stimuli.}
#'   \item{mrt_mixed}{Mean reaction time of Mixed stimuli.}
#'   \item{mrt_mixadv}{Mean reaction decrease of Mixed stimuli compared to other two types of stimuli.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
multisense <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "RT")))) {
    warning("`Type` and `RT` variables are required.")
    return(
      data.frame(
        mrt_image = NA_real_,
        mrt_sound = NA_real_,
        mrt_mixed = NA_real_,
        mrt_mixadv = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::filter(.data$RT > 100) %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(mrt = mean(.data$RT)) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "mrt") %>%
    dplyr::transmute(
      mrt_image = .data$Image,
      mrt_sound = .data$Sound,
      mrt_mixed = .data$Mixed,
      mrt_mixadv = (.data$Image + .data$Sound) / 2 - .data$Mixed,
      is_normal = TRUE
    )
}
