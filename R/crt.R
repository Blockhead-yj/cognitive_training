#' Calculates index scores for Choice Reaction Time game
#'
#' Now we majorly consider two indices: mean reaction time and count of correct
#' responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mrt}{Mean reaction time}
#'   \item{nc}{Count of correct responses}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
crt <- function(data, ...) {
  if (!all(utils::hasName(data, c("ACC", "RT")))) {
    warning("`ACC` and `RT` variables are required.")
    return(
      data.frame(
        mrt = NA_real_,
        nc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::summarise(
      mrt = mean(.data$RT[.data$ACC == 1]),
      nc = sum(.data$ACC == 1),
      is_normal = TRUE
    )
}
