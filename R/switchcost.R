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
#'   \item{count_pure}{Count of correct responses per minute for pure blocks.}
#'   \item{count_mixed}{Count of correct responses per minute for mixed blocks.}
#'   \item{switch_cost_gen_count}{General switch cost (based on count of correct responses).}
#'   \item{mrt_pure}{Mean reaction time for non-mixed blocks.}
#'   \item{mrt_repeat}{Mean reaction time for repeat trials.}
#'   \item{mrt_switch}{Mean reaction time for switch trials.}
#'   \item{switch_cost_gen_rt}{General switch cost (based on mean reation times).}
#'   \item{switch_cost_spe_rt}{Specific switch cost (based on mean reation times).}
#'   \item{is_normal}{Checking result whether the data is normal.} }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
switchcost <- function(data, ...) {
  if (!all(utils::hasName(data, c("Block", "Type", "ACC", "RT")))) {
    warning("`Block`, `Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        count_correct = NA_real_,
        count_pure = NA_real_,
        count_mixed = NA_real_,
        switch_cost_gen_count = NA_real_,
        mrt_pure = NA_real_,
        mrt_repeat = NA_real_,
        mrt_switch = NA_real_,
        switch_cost_gen_rt = NA_real_,
        switch_cost_spe_rt = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(
      type_adj = dplyr::case_when(
        .data$Type %in% c("Repeat", "Switch") ~ .data$Type,
        .data$Type == "Filler" ~ "",
        TRUE ~  .data$Task
      ),
      acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L)
    )
  count_correct <- data_adj %>%
    dplyr::summarise(count_correct = sum(.data$acc_adj == 1))
  n_blocks <- dplyr::n_distinct(data_adj$Block)
  if (n_blocks == 5) {
    switch_cost_count <- data_adj %>%
      dplyr::group_by(.data$Block) %>%
      dplyr::summarise(count_correct = sum(.data$acc_adj == 1)) %>%
      tidyr::pivot_wider(names_from = "Block", values_from = "count_correct") %>%
      dplyr::transmute(
        count_pure = mean(c(.data$`1`, .data$`2`)),
        count_mixed = mean(c(.data$`3`, .data$`4`, .data$`5`)),
        switch_cost_gen_count = .data$count_pure - .data$count_mixed
      )
  } else if (n_blocks == 6) {
    switch_cost_count <- data_adj %>%
      dplyr::group_by(.data$Block) %>%
      dplyr::summarise(count_correct = sum(.data$acc_adj == 1)) %>%
      tidyr::pivot_wider(names_from = "Block", values_from = "count_correct") %>%
      dplyr::transmute(
        count_pure = sum(c(.data$`1`, .data$`2`)),
        count_mixed = mean(c(.data$`3`, .data$`4`, .data$`5`, .data$`6`)),
        switch_cost_gen_count = .data$count_pure - .data$count_mixed
      )
  } else {
    switch_cost_count <- data.frame(
      count_pure = NA_real_,
      count_mixed = NA_real_,
      switch_cost_gen_count = NA_real_
    )
  }
  switch_cost_rt <- data_adj %>%
    dplyr::filter(.data$type_adj != "") %>%
    dplyr::group_by(.data$type_adj) %>%
    dplyr::summarise(mrt = mean(.data$RT[.data$acc_adj == 1])) %>%
    tidyr::pivot_wider(names_from = "type_adj", values_from = "mrt") %>%
    dplyr::transmute(
      mrt_pure = (.data$Color + .data$Shape) / 2,
      mrt_repeat = .data$Repeat,
      mrt_switch = .data$Switch,
      switch_cost_gen_rt = .data$mrt_repeat - .data$mrt_pure,
      switch_cost_spe_rt = .data$mrt_switch - .data$mrt_repeat
    )
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(count_correct, switch_cost_count, switch_cost_rt, is_normal)
}
