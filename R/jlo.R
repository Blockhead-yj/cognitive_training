#' Calculates index scores for JLO game.
#'
#' This is just to find out the count of correct responses.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{count_correct}{Count of correct responses.}
#'   \item{sum_error}{Sum of the angle deviations.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang !!
#' @export
jlo <- function(data, ...) {
  if (!all(utils::hasName(data, c("Angle", "Resp", "ACC")))) {
    warning("`Angle`, `Resp` and `ACC` variables are required.")
    return(
      data.frame(
        count_correct = NA_real_,
        sum_error = NA_real_,
        is_normal = FALSE
      )
    )
  }
  count_correct <- data %>%
    dplyr::summarise(count_correct = sum(.data$ACC == 1))
  sum_error <- data %>%
    dplyr::mutate(
      resp_adj = purrr::map_dbl(
        .data$Resp,
        ~ strsplit(.x, "-") %>%
          unlist() %>%
          stringr::str_replace_all(c("Left" = "1", "Right" = "-1")) %>%
          as.numeric() %>%
          sum()
      ),
      resp_angle = dplyr::case_when(
        # when rotating larger than a right angle, adjusting it
        .data$resp_adj > 15 ~ .data$resp_adj * 6 - 180,
        .data$resp_adj < -15 ~ .data$resp_adj * 6 + 180,
        TRUE ~ .data$resp_adj * 6
      )
    ) %>%
    dplyr::summarise(sum_error = sum(abs(.data$Angle - .data$resp_angle)))
  cbind(count_correct, sum_error, is_normal = TRUE)
}
