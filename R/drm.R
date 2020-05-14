#' Calculates index scores for DRM paradigm game
#'
#' Two major indices are included: fm_ratio (false memory ratio, i.e.,
#' p(old|lure) - p(old|foil)) and fm_dprime (false memory d', i.e.,
#' z(p(old|lure)) - z(p(old|foil)))
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{fm_ratio}{False memory ratio.}
#'   \item{fm_dprime}{False memory d'.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
drm <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "RT", "ACC")))) {
    warning("`Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        fm_ratio = NA_real_,
        fm_dprime = NA_real_,
        is_normal = FALSE
      )
    )
  }
  fm <- data %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(p_old = sum(.data$Resp == "Old") / dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "p_old") %>%
    dplyr::transmute(
      fm_ratio = .data$Lure - .data$Foil,
      fm_dprime = stats::qnorm(.data$Lure) - stats::qnorm(.data$Foil)
    )
  is_normal <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L)) %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(fm, is_normal)
}
