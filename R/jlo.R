#' Calculates index scores for JLO game.
#'
#' Count of correct responses and other angle deviations indices.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{nc}{Count of correct responses.}
#'   \item{ne}{Sum of the angle deviations.}
#'   \item{ne_ln}{Sum of the log of angle deviations.}
#'   \item{ne_sqrt}{Sum of the square root of angle deviations.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
jlo <- function(data, ...) {
  if (!all(utils::hasName(data, c("Angle", "Resp", "ACC")))) {
    warning("`Angle`, `Resp` and `ACC` variables are required.")
    return(
      data.frame(
        nc = NA_real_,
        ne = NA_real_,
        ne_ln = NA_real_,
        ne_sqrt = NA_real_,
        is_normal = FALSE
      )
    )
  }
  nc <- data %>%
    dplyr::summarise(nc = sum(.data$ACC == 1))
  ne <- data %>%
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
    dplyr::summarise(
      ne = sum(abs(.data$Angle - .data$resp_angle)),
      ne_ln = sum(log(abs(.data$Angle - .data$resp_angle) + 1)),
      ne_sqrt = sum(sqrt(abs(.data$Angle - .data$resp_angle)))
    )
  cbind(nc, ne, is_normal = TRUE)
}
