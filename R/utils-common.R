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
