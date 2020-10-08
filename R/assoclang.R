#' Calculates index scores for AssocLang game.
#'
#' The count of correct responses is returned.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{count_correct}{Count of correct responses.}
#'   \item{pc}{Percent of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @importFrom rlang .data
#' @export
assoclang <- function(data, ...) {
  if (!all(utils::hasName(data, "Correctness"))) {
    warning("`Correctness` variable is required.")
    return(
      data.frame(
        count_correct = NA_real_,
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
  count_correct <- sum(correctness, na.rm = TRUE)
  pc <- mean(correctness, na.rm = TRUE)
  data.frame(count_correct, pc, is_normal = TRUE)
}
