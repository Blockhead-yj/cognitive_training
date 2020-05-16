#' Calculates index scores for games related to conflict effect
#'
#' Count of correct responses and congruency effect of correct response and
#' reaction time are all included.
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{count_correct}{Count of correct responses.}
#'   \item{switch_cost_gen}{General switch cost (based on mean reation times).}
#'   \item{switch_cost_spe}{Specific switch cost (based on mean reation times).}
#'   \item{is_normal}{Checking result whether the data is normal.} }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
switchcost <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "ACC", "RT")))) {
    warning("`Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        count_correct = NA_real_,
        switch_cost_gen = NA_real_,
        switch_cost_spe = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L))
  count_correct <- data_adj %>%
    dplyr::summarise(count_correct = sum(.data$acc_adj == 1))
  switch_cost_all <- data_adj %>%
    dplyr::mutate(
      type_adj = dplyr::case_when(
        .data$Type == "" ~ .data$Task,
        .data$Type == "Filler" ~ "",
        TRUE ~ .data$Type
      )
    ) %>%
    dplyr::filter(.data$type_adj != "") %>%
    dplyr::group_by(.data$type_adj) %>%
    dplyr::summarise(mrt = mean(.data$RT[.data$acc_adj == 1])) %>%
    tidyr::pivot_wider(names_from = "type_adj", values_from = "mrt") %>%
    dplyr::transmute(
      switch_cost_gen = .data$Repeat - (.data$Color + .data$Shape) / 2,
      switch_cost_spe = .data$Switch - .data$Repeat
    )
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(count_correct, switch_cost_all, is_normal)
}
