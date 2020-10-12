#' Calculates index scores for games related to conflict effect
#'
#' Count of correct responses and congruency effect of correct response and
#' reaction time are all included.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{rc_pure}{Count of correct responses per minute for pure blocks.}
#'   \item{rc_mixed}{Count of correct responses per minute for mixed blocks.}
#'   \item{switch_cost_rc_gen}{General switch cost (based on count of correct
#'     responses).}
#'   \item{mrt_pure}{Mean reaction time for non-mixed blocks.}
#'   \item{mrt_repeat}{Mean reaction time for repeat trials.}
#'   \item{mrt_switch}{Mean reaction time for switch trials.}
#'   \item{switch_cost_gen_rt}{General switch cost (based on mean reation
#'     times).}
#'   \item{switch_cost_spe_rt}{Specific switch cost (based on mean reation
#'     times).}
#'   \item{nc}{Count of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
switchcost <- function(data, ...) {
  if (!all(utils::hasName(data, c("Block", "Task", "Type", "ACC", "RT")))) {
    warning("`Block`, `Task`, `Type`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        rc_pure = NA,
        rc_mixed = NA,
        switch_cost_rc_gen = NA,
        mrt_pure = NA,
        mrt_repeat = NA,
        mrt_switch = NA,
        switch_cost_rt_gen = NA,
        switch_cost_rt_spe = NA,
        nc = NA,
        is_normal = FALSE
      )
    )
  }
  # summarize information of each block
  block_info <- data %>%
    dplyr::mutate(n_blocks = dplyr::n_distinct(.data$Block)) %>%
    dplyr::group_by(.data$n_blocks, .data$Block) %>%
    dplyr::summarise(
      has_no_response = all(.data$ACC == -1),
      type_block = ifelse(
        is.na(.data$Type) |
          .data$Type %in% c("preswitch", "postswitch", "Pure", ""),
        "pure", "mixed"
      ) %>%
        unique(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dur = dplyr::case_when(
        .data$n_blocks == 5 ~ 1,
        .data$n_blocks >= 6 & .data$type_block == "pure" ~ 0.5,
        .data$n_blocks >= 6 & .data$type_block == "mixed" ~ 1,
        TRUE ~ NA_real_
      )
    )
  data_adj <- data %>%
    # set as wrong for responses that are too quick
    dplyr::mutate(acc_adj = ifelse(.data$RT >= 100, .data$ACC, 0))
  switch_cost <- calc_switch_cost(
    data_adj, block_info,
    name_acc = "acc_adj"
  )
  nc_and_validation <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(
      .data$nc,
      is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5) ||
        any(block_info$has_no_response)
    )
  cbind(switch_cost, nc_and_validation)
}
