#' Calculates index scores for Location Memory (so-called Black Hole) games.
#'
#' Mean distance and percentage of correct responses are returned.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mean_dist}{Mean distance.}
#'   \item{pc}{Percentage of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
locmem <- function(data, ...) {
  if (!all(utils::hasName(data, "RespLocDist"))) {
    warning("`RespLocDist` variable is required.")
    return(
      data.frame(
        mean_dist = NA_real_,
        pc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  delim <- "-"
  all_dists <- data %>%
    dplyr::pull("RespLocDist") %>%
    paste(collapse = delim) %>%
    strsplit(delim) %>%
    unlist() %>%
    as.numeric()
  data.frame(
    mean_dist = mean(all_dists),
    pc = mean(all_dists == 0),
    is_normal = TRUE
  )
}
