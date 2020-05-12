#' Calculates index scores for BART game specifically
#'
#' The adjusted and unadjusted BART scores are both returned.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{mean_hits}{Mean of hits for balloons not exploded.}
#'   \item{mean_hits_raw}{Mean of hits for all balloons.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @export
bart <- function(data, ...) {
  if (!all(utils::hasName(data, c("NHit", "Feedback")))) {
    warning("`NHit` and `Feedback` variables are required.")
    return(
      data.frame(
        mean_hits = NA_real_,
        mean_hits_raw = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data %>%
    dplyr::summarise(
      mean_hits = mean(NHit[Feedback == 1]),
      mean_hits_raw = mean(NHit),
      is_normal = TRUE
    )
}
