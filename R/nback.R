#' Calculates index scores for N Back games.
#'
#' Several values including percentage of correct responses (pc), mean reaction
#' time (mrt), sensitivity index (i.e, d') and bias (c).
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{pc}{Percentage of correct responses.}
#'   \item{mrt}{Mean reaction time.}
#'   \item{dprime}{Sensitivity index.}
#'   \item{c}{Bias.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
nback <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "RT", "ACC")))) {
    warning("`Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        mrt = NA_real_,
        dprime = NA_real_,
        c = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(
      type_adj = dplyr::if_else(.data$Type == "Change", "s", "n"),
      acc_adj = dplyr::if_else(.data$RT <= 100, 0L, .data$ACC)
    )
  basic <- data_adj %>%
    dplyr::summarise(
      pc = mean(.data$acc_adj == 1),
      mrt = mean(.data$RT[.data$acc_adj == 1])
    )
  sdt <- data_adj %>%
    dplyr::group_by(.data$type_adj) %>%
    dplyr::summarise(
      n = dplyr::n(),
      pc = mean(.data$acc_adj == 1)
    ) %>%
    dplyr::mutate(
      pc_adj = dplyr::case_when(
        .data$pc == 0 ~ 1 / (2 * .data$n),
        .data$pc == 1 ~ 1 - 1 / (2 * .data$n),
        TRUE ~ .data$pc
      )
    ) %>%
    dplyr::select(.data$type_adj, .data$pc_adj) %>%
    tidyr::pivot_wider(names_from = "type_adj", values_from = "pc_adj") %>%
    dplyr::transmute(
      dprime = stats::qnorm(.data$s) + stats::qnorm(.data$n),
      c = -(stats::qnorm(.data$s) - stats::qnorm(.data$n)) / 2
    )
  is_normal <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(basic, sdt, is_normal)
}
