#' Calculates index scores for Number Line Estimation game.
#'
#' Now the mean absolute error (mean_err) is calculated. Future work will
#' be to do model fitting.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{mean_err}{Mean percent absolute error.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
nle <- function(data, ...) {
  if (!all(utils::hasName(data, c("Number", "Resp")))) {
    warning("`Number` and `Resp` variables are required.")
    return(
      data.frame(
        mean_err = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::mutate(err = abs(.data$Number - .data$Resp)) %>%
    dplyr::summarise(mean_err = mean(.data$err), is_normal = TRUE)
}
