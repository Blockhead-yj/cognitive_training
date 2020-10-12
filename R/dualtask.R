#' Calculates index scores for dual task paradigm
#'
#' In the simplified (only dual task block included) dual task paradigm, users
#' are required to complete two tasks spontaneously, one at the left side, and
#' the other at the right side. Mean reaction time (`mrt`), count of
#' correct responses (`nc`) and sensitivity index (`dprime`) for left side,
#' right side and both side are calculated.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{mrt_all}{Mean reaction time for all correct trials.}
#'   \item{mrt_left}{Mean reaction time for correct trials of left side.}
#'   \item{mrt_right}{Mean reaction time for correct trials of right side.}
#'   \item{nc_all}{Number of correct responses of both sides.}
#'   \item{nc_left}{Number of correct responses of left side.}
#'   \item{nc_right}{Number of correct responses of right side.}
#'   \item{dprime_all}{Sensitivity index (d') of both sides.}
#'   \item{dprime_left}{Sensitivity index (d') of left side.}
#'   \item{dprime_right}{Sensitivity index (d') of right side.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
dualtask <- function(data, ...) {
  . <- NULL
  if (!all(utils::hasName(data, c("Side", "StimType", "ACC", "RT")))) {
    warning("`Side`, `StimType`, `ACC` and `RT` variables are required.")
    return(
      data.frame(
        mrt_all = NA,
        mrt_left = NA,
        mrt_right = NA,
        nc_all = NA,
        nc_left = NA,
        nc_right = NA,
        dprime_all = NA,
        dprime_left = NA,
        dprime_right = NA,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(
      acc_adj = dplyr::if_else(
        .data$RT >= 100 | .data$StimType == "NonTarget",
        .data$ACC, 0L
      )
    )
  # dummy combination of side-wise data and full data
  data_dummy_cmb <- list(
    sidewise = data_adj,
    both = data_adj
  ) %>%
    dplyr::bind_rows(.id = "ind_type") %>%
    dplyr::mutate(
      Side = dplyr::if_else(
        .data$ind_type == "sidewise",
        .data$Side, "all"
      )
    )
  # indices calculated based on accuracy
  ind_from_acc <- data_dummy_cmb %>%
    dplyr::group_by(.data$Side, .data$StimType) %>%
    dplyr::summarise(
      nt = dplyr::n(),
      nc = sum(.data$acc_adj),
      pc = .data$nc / .data$nt
    ) %>%
    # correct perfect responses
    dplyr::mutate(
      pc = dplyr::case_when(
        .data$pc == 0 ~ 1 / (2 * .data$nt),
        .data$pc == 1 ~ 1 - 1 / (2 * .data$nt),
        TRUE ~ .data$pc
      )
    ) %>%
    dplyr::summarise(
      nc = sum(.data$nc),
      dprime = sum(stats::qnorm(.data$pc))
    )
  # indices calculated based on reaction times
  ind_from_rt <- data_dummy_cmb %>%
    dplyr::filter(.data$acc_adj == 1, .data$StimType == "Target") %>%
    dplyr::group_by(.data$Side) %>%
    dplyr::summarise(
      mrt = mean(.data$RT),
      .groups = "drop"
    )
  ind_from_rt %>%
    dplyr::inner_join(ind_from_acc, by = "Side") %>%
    # put indices in one row and rename to lowercase ones
    tidyr::pivot_wider(
      names_from = "Side",
      values_from = c("mrt", "nc", "dprime")
    ) %>%
    dplyr::rename_with(tolower) %>%
    # no proper way to validate user's responses
    dplyr::mutate(is_normal = TRUE)
}
