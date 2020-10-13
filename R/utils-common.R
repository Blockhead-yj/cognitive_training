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
#' @param data Required. A `data.frame` from which the indices are calculated.
#' @param config_block Required. A `data.frame` contains the configurations of
#'   each block. At least contains these variables:
#'   * Same name as `name_block` argument: The block identifier.
#'   * `has_no_response`: Logical value indicating whether this block has no
#'     valid responses or not.
#'   * `type_block`: The type of each block: "pure" or "mixed".
#'   * `dur`: The duration in minutes for each block.
#' @param name_block The name of the variable in `data` storing block number.
#' @param name_task The name of variable in `data` storing task info.
#' @param name_switch The name of the variable in `data` storing switch info.
#' @param name_rt The name of the variable in `data` storing reaction time data.
#' @param name_acc The name of the variable in `data` storing accuracy data.
#' @param values_mixed The values of the variable `name_switch` in mixed
#'   blocks, in which the first is about 'repeat', and the second about
#'   'switch'.
calc_switch_cost <- function(data,
                             config_block,
                             name_block = "Block",
                             name_task = "Task",
                             name_switch = "Type",
                             name_rt = "RT",
                             name_acc = "ACC",
                             values_mixed = c("Repeat", "Switch")) {
  switch_cost_count <- data %>%
    dplyr::left_join(config_block, by = name_block) %>%
    dplyr::group_by(.data$type_block, .data[[name_block]]) %>%
    dplyr::filter(!.data$has_no_response) %>%
    dplyr::summarise(rc = sum((.data[[name_acc]] == 1) / .data$dur)) %>%
    dplyr::summarise(rc = mean(.data$rc)) %>%
    tidyr::pivot_wider(
      names_from = "type_block",
      values_from = "rc",
      names_prefix = "rc_"
    ) %>%
    dplyr::mutate(
      switch_cost_rc_gen = .data$rc_pure - .data$rc_mixed
    )
  switch_cost_rt <- data %>%
    dplyr::left_join(config_block, by = name_block) %>%
    dplyr::mutate(
      type_rt = dplyr::if_else(
        .data$type_block == "pure",
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
#' Get the matched variables for input data
#'
#' Input data require several variables to be existed, and some variables can
#' have alternatives. Here we check if all the variables exist, and if only one
#' of the alternatives exist. If the check is passed, the matched result will be
#' returned, or a `FALSE` value will be returned.
#'
#' @param data Required. Raw data, a `data.frame`.
#' @param vars_required Required. A `data.frame` containing the configuration of
#'   required variables. At least contains `name` variable, which lists all of
#'   the required variable names, each element of which is a character vector
#'   containing all the possible names.
match_data_vars <- function(data, vars_required) {
  var_chk_result <- vars_required %>%
    dplyr::mutate(
      purrr::map_df(
        .data$name,
        ~ {
          match_result <- utils::hasName(data, .x)
          tibble(
            is_found = any(match_result),
            is_confused = sum(match_result) > 1,
            matched =
              # the match can be of length 0, so use `if` instead of `if_else`
              if (.data$is_confused | (!.data$is_found)) {
                NA
              } else {
                .x[match_result]
              }
          )
        }
      )
    )
  if (!all(var_chk_result$is_found)) {
    warning("At least one of the required variables are missing.")
    return(FALSE)
  }
  if (any(var_chk_result$is_confused)) {
    warning("At least one of the variables have more than one match.")
    return(FALSE)
  }
  return(var_chk_result)
}
