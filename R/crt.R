#' Calculates index scores for Choice Reaction Time game
#'
#' Now we majorly consider two indices: mean reaction time and count of correct
#' responses.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{mrt}{Mean reaction time}
#'   \item{count_correct}{Count of correct responses}
#' }
#' @importFrom magrittr %>%
#' @export
crt <- function(data, ...) {
  if (!all(utils::hasName(data, c("ACC", "RT")))) {
    warning("`ACC` and `RT` variables are required.")
    return(data.frame(mrt = NA_real_, count_correct = NA_real_))
  }
  data %>%
    dplyr::summarise(
      mrt = mean(.data$RT[.data$ACC == 1]),
      count_correct = sum(.data$ACC == 1)
    )
}
