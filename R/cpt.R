#' Calculates index scores for Continuous Performance Test game.
#'
#' Many indices are returned: d', c (i.e., bias), hits, commissions, ommissions,
#' mean reaction time (mrt), standard deviation of reaction times (rtsd).
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{dprime}{Sensitivity (d').}
#'   \item{c}{Bias index.}
#'   \item{hits}{Number of hits.}
#'   \item{commissions}{Number of errors caused by action.}
#'   \item{omissions}{Number of errors caused by inaction.}
#'   \item{count_error}{Count of incorrect responses.}
#'   \item{mrt}{Mean reaction time of hits.}
#'   \item{rtsd}{Standard deviation of reaction times of hits.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
cpt <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "RT", "ACC")))) {
    warning("`Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        dprime = NA_real_,
        c = NA_real_,
        hits = NA_real_,
        commissions = NA_real_,
        omissions = NA_real_,
        count_error = NA_real_,
        mrt = NA_real_,
        rtsd = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(
      type_adj = dplyr::if_else(.data$Type == "Target", "s", "n"),
      acc_adj = dplyr::if_else(.data$RT < 100 & .data$type_adj == "s", 0L, .data$ACC)
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
  counts <- data_adj %>%
    dplyr::group_by(.data$type_adj) %>%
    dplyr::summarise(
      nc = sum(.data$acc_adj == 1),
      ne = sum(.data$acc_adj == 0)
    ) %>%
    tidyr::pivot_wider(names_from = "type_adj", values_from = c("nc", "ne")) %>%
    dplyr::transmute(
      hits = .data$nc_s,
      commissions = .data$ne_n,
      omissions = .data$ne_s,
      count_error = .data$ne_n + .data$ne_s
    )
  rt <- data_adj %>%
    dplyr::filter(.data$acc_adj == 1 & .data$type_adj == "s") %>%
    dplyr::summarise(
      mrt = mean(.data$RT),
      rtsd = stats::sd(.data$RT)
    )
  is_normal <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(sdt, counts, rt, is_normal)
}
