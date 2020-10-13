#' Calculates index scores for complex switch games
#'
#' This is just to find out the count of correct responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A [tibble][tibble::tibble-package] with the following variables:
#'   \item{mrt_con}{Mean reaction time for congruent trials.}
#'   \item{mrt_inc}{Mean reaction time for incogruent trials.}
#'   \item{cong_eff_rt}{Congruency effect of reaction time (RT), i.e., RT
#'     incongruency - RT congruency.}
#'   \item{pc_con}{Percent of correct for congruent trials.}
#'   \item{pc_inc}{Percent of correct for incogruent trials.}
#'   \item{cong_eff_pc}{Congruency effect of percent of correct (PC), i.e., PC
#'     congruency - PC incongruency.}
#'   \item{rc_mixed}{Count of correct responses per minute for mixed blocks.}
#'   \item{rc_pure}{Count of correct responses per minute for pure blocks.}
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
complexswitch <- function(data, ...) {
  vars_output <- c(
    "mrt_con", "mrt_inc", "cong_eff_rt",
    "pc_con", "pc_inc", "cong_eff_pc",
    "rc_mixed", "rc_pure", "switch_cost_rc_gen",
    "mrt_pure", "mrt_repeat", "mrt_switch",
    "switch_cost_rt_gen", "switch_cost_rt_spe",
    "nc"
  )
  vars_required <- tibble::tribble(
    ~ field, ~ name,
    "name_block", "Block",
    "name_cong", "StimType",
    "name_task", c("Task", "Sex"),
    "name_switch", "TaskType",
    "name_acc", c("Acc", "ACC"),
    "name_rt", "RT"
  )
  vars_chk_result <- match_data_vars(data, vars_required)
  if (isFALSE(vars_chk_result)) {
    return(
      rlang::set_names(
        rep(NA, length(vars_output)),
        nm = vars_output
      ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(is_normal = FALSE)
    )
  }
  vars_matched <- vars_chk_result %>%
    dplyr::select(.data$field, .data$matched) %>%
    tibble::deframe()
  data <- data %>%
    dplyr::mutate(
      acc_adj = dplyr::if_else(
        !!sym(vars_matched[["name_rt"]]) >= 100,
        !!sym(vars_matched[["name_acc"]]), 0L
      )
    )
  # calculate congruency effect
  cong_eff <- calc_cong_eff(
    data,
    name_cong = vars_matched[["name_cong"]],
    name_acc = "acc_adj"
  )
  # calculate switch cost
  block_info <- data %>%
    dplyr::mutate(
      n_blocks = dplyr::n_distinct(!!sym(vars_matched[["name_block"]]))
    ) %>%
    dplyr::group_by(.data$n_blocks, !!sym(vars_matched[["name_block"]])) %>%
    dplyr::summarise(
      has_no_response = all(!!sym(vars_matched[["name_acc"]]) == -1),
      type_block = ifelse(
        all(!!sym(vars_matched[["name_switch"]]) %in% c("Pure", "")),
        "pure", "mixed"
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dur = dplyr::if_else(.data$type_block == "pure", 0.5, 1))
  switch_cost <- calc_switch_cost(
    data, block_info,
    name_task = vars_matched[["name_task"]],
    name_switch = vars_matched[["name_switch"]],
    name_acc = vars_matched[["name_acc"]]
  )
  nc_and_validation <- data %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(
      .data$nc,
      is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5) ||
        any(block_info$has_no_response)
    )
  tibble(cong_eff, switch_cost, nc_and_validation)
}
