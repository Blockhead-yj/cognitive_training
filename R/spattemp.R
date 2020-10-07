#' Calculates index scores for Spatiotemporal Memory (so-called Camera) games.
#'
#' Mean distance and percentage of correct responses are returned. The only
#' difference with \code{\link{locmem}} is that this function takes order
#' accuracy (thus temporal) into consideration.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{mean_dist}{Mean distance.}
#'   \item{pc}{Percentage of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom rlang .data
#' @export
spattemp <- function(data, ...) {
  if (!all(utils::hasName(data, c("RespAccOrder", "RespLocDist")))) {
    warning("`RespAccOrder` and `RespLocDist` variables are required.")
    return(
      data.frame(
        mean_dist = NA_real_,
        pc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  delim <- "-"
  all_ordacc <- data %>%
    dplyr::pull("RespAccOrder") %>%
    paste(collapse = delim) %>%
    strsplit(delim) %>%
    unlist() %>%
    as.numeric()
  all_dists <- data %>%
    dplyr::pull("RespLocDist") %>%
    paste(collapse = delim) %>%
    strsplit(delim) %>%
    unlist() %>%
    as.numeric()
  data.frame(
    mean_dist = mean(all_dists * all_ordacc),
    pc = mean(all_dists == 0 & all_ordacc == 1),
    is_normal = TRUE
  )
}
