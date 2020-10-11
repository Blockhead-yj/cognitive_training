#' Congruency effect
#'
#' Utility function to calculate indices related to congruency effect.
#'
#' @param data A `data.frame` from which the indices are calculated.
#' @param name_cong The name of the variable in `data` storing the congruency
#'   info.
#' @param name_rt The name of the variable in `data` storing reaction time data.
#' @param name_acc The name of the variable in `data` storing accuracy data.
#' @param values_cong The values of the congruency info variable, in which the
#'   first is about 'congruent', and the second about 'incongruent'.
#' @importFrom rlang .data
calc_cong_eff <- function(data,
                          name_cong = "Type",
                          name_rt = "RT",
                          name_acc = "ACC",
                          values_cong = c("Congruent", "Incongruent")) {
  data %>%
    dplyr::group_by(.data[[name_cong]]) %>%
    dplyr::summarise(
      mrt = mean(.data[[name_rt]][.data[[name_acc]] == 1]),
      pc = mean(.data[[name_acc]] == 1)
    ) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(name_cong),
      values_from = c("mrt", "pc")
    ) %>%
    dplyr::transmute(
      mrt_con = .data[[paste("mrt", values_cong[[1]], sep = "_")]],
      mrt_inc = .data[[paste("mrt", values_cong[[2]], sep = "_")]],
      cong_eff_rt = .data$mrt_inc - .data$mrt_con,
      pc_con = .data[[paste("pc", values_cong[[1]], sep = "_")]],
      pc_inc = .data[[paste("pc", values_cong[[2]], sep = "_")]],
      cong_eff_pc = .data$pc_con - .data$pc_inc
    )
}
#' Switch cost
#'
#' Utility function to calculate indices related to switch cost.
#'
#' @param data A `data.frame` from which the indices are calculated.
#' @param name_block The name of the variable in `data` storing block number.
#' @param name_task The name of variable in `data` storing task info.
#' @param name_switch The name of the variable in `data` storing switch info.
#' @param name_rt The name of the variable in `data` storing reaction time data.
#' @param name_acc The name of the variable in `data` storing accuracy data.
#' @param values_pure The values of the variable `name_switch` in pure blocks.
#' @param values_mixed The values of the variable `name_switch` in mixed
#'   blocks, in which the first is about 'repeat', and the second about
#'   'switch'.
#' @param dur_pure The duration in minutes for pure blocks.
#' @param dur_mixed The duration in minutes for mixed blocks.
#' @importFrom rlang .data
calc_switch_cost <- function(data,
                             name_block = "Block",
                             name_task = "Task",
                             name_switch = "Type",
                             name_rt = "RT",
                             name_acc = "ACC",
                             values_pure = c("", "Pure"),
                             values_mixed = c("Repeat", "Switch"),
                             dur_pure = 0.5,
                             dur_mixed = 1) {
  data_tmp <- data %>%
    dplyr::mutate(
      type_block = dplyr::if_else(
        .data[[name_switch]] %in% values_pure,
        "pure", "mixed"
      )
    )
  switch_cost_count <- data_tmp %>%
    dplyr::group_by(.data$type_block, .data[[name_block]]) %>%
    dplyr::summarise(nc = sum(.data[[name_acc]])) %>%
    dplyr::transmute(
      # rate of correct responses per minute
      rc = dplyr::case_when(
        .data$type_block == "pure" ~ .data$nc / dur_pure,
        .data$type_block == "mixed" ~ .data$nc / dur_mixed
      )
    ) %>%
    dplyr::summarise(rc = mean(.data$rc)) %>%
    tidyr::pivot_wider(
      names_from = "type_block", values_from = "rc", names_prefix = "rc_"
    ) %>%
    dplyr::mutate(
      switch_cost_rc_gen = .data$rc_pure - .data$rc_mixed
    )
  switch_cost_rt <- data_tmp %>%
    # include both valid mixed and pure (used in general switch cost) trials
    dplyr::filter(.data[[name_switch]] %in% c(values_mixed, values_pure)) %>%
    dplyr::mutate(
      type_rt = dplyr::if_else(
        .data[[name_switch]] %in% values_pure,
        .data[[name_task]],
        .data[[name_switch]]
      )
    ) %>%
    dplyr::group_by(.data$type_block, .data$type_rt) %>%
    dplyr::summarise(
      mrt = mean(.data[[name_rt]][.data[[name_acc]] == 1]),
      .groups = "drop"
    ) %>%
    dplyr::summarise(
      mrt_pure = mean(.data$mrt[.data$type_block == "pure"]),
      mrt_repeat = .data$mrt[.data$type_rt == values_mixed[[1]]],
      mrt_switch = .data$mrt[.data$type_rt == values_mixed[[2]]],
      switch_cost_rt_gen = .data$mrt_repeat - .data$mrt_pure,
      switch_cost_rt_spe = .data$mrt_switch - .data$mrt_repeat
    )
  cbind(switch_cost_count, switch_cost_rt)
}
