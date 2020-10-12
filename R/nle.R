#' Calculates index scores for Number Line Estimation game.
#'
#' Now the mean absolute error (mean_err) is calculated. Future work will
#' be to do model fitting.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mean_err}{Mean absolute error.}
#'   \item{mean_logerr}{Mean log absolute error.}
#'   \item{mean_sqrterr}{Mean square root of absolute error.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
nle <- function(data, ...) {
  if (!all(utils::hasName(data, c("Number", "Resp")))) {
    warning("`Number` and `Resp` variables are required.")
    return(
      data.frame(
        mean_err = NA_real_,
        mean_logerr = NA_real_,
        mean_sqrterr = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::mutate(err = abs(.data$Number - .data$Resp)) %>%
    dplyr::summarise(
      mean_err = mean(.data$err),
      mean_logerr = mean(log(.data$err + 1)),
      mean_sqrterr = mean(sqrt(.data$err)),
      is_normal = TRUE
    )
}
