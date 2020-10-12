#' Calculates index scores for AssocLang game.
#'
#' The count of correct responses is returned.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{nc}{Count of correct responses.}
#'   \item{pc}{Percent of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
assoclang <- function(data, ...) {
  .Deprecated("countcorrect")
  if (!all(utils::hasName(data, "Correctness"))) {
    warning("`Correctness` variable is required.")
    return(
      data.frame(
        nc = NA_real_,
        pc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  correctness <- gsub("NULL", "", data$Correctness) %>%
    paste(collapse = "-") %>%
    strsplit("-") %>%
    unlist() %>%
    as.numeric()
  nc <- sum(correctness, na.rm = TRUE)
  pc <- mean(correctness, na.rm = TRUE)
  data.frame(nc, pc, is_normal = TRUE)
}
