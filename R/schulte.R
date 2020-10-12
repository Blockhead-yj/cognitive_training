#' Calculates index scores for Schulte Grid games
#'
#' The net count of correct responses (net_cor) is returned, which is just the
#' difference of the count of correct responses and error responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{net_cor}{Net correct count.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
schulte <- function(data, ...) {
  if (!all(utils::hasName(data, c("NCorrect", "NError")))) {
    warning("`NCorrect` and `NError` variables are required.")
    return(
      data.frame(
        net_cor = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::summarise(
      net_cor = sum(.data$NCorrect) - sum(.data$NError),
      is_normal = TRUE
    )
}
