#' Calculates index scores for Simple Reaction Time game
#'
#' Mean reaction time is returned.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{mrt}{Mean reaction time}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
srt <- function(data, ...) {
  if (!all(utils::hasName(data, "RT"))) {
    warning("`RT` variable is required.")
    return(
      data.frame(
        mrt = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::summarise(
      mrt = mean(.data$RT[.data$RT > 100]),
      is_normal = TRUE
    )
}
