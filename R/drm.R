#' Calculates index scores for DRM paradigm game
#'
#' Two major indices are included: fm_ratio (false memory ratio, i.e.,
#' p(old|lure) - p(old|foil)) and fm_dprime (false memory d', i.e.,
#' z(p(old|lure)) - z(p(old|foil)))
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{pc}{Percent of correct responses.}
#'   \item{p_old_lure}{Percent of old response for "lure" stimuli.}
#'   \item{p_old_foil}{Percent of old response for "foil" stimuli.}
#'   \item{fm_ratio}{False memory ratio.}
#'   \item{fm_dprime}{False memory d'.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
drm <- function(data, ...) {
  if (!all(utils::hasName(data, c("Type", "RT", "ACC")))) {
    warning("`Type`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        p_old_lure = NA_real_,
        p_old_foil = NA_real_,
        fm_ratio = NA_real_,
        fm_dprime = NA_real_,
        is_normal = FALSE
      )
    )
  }
  pc_all <- data %>%
    dplyr::summarise(pc = mean(.data$ACC == 1))
  fm <- data %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarise(p_old = sum(.data$Resp == "Old") / dplyr::n()) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "p_old") %>%
    dplyr::transmute(
      p_old_lure = .data$Lure,
      p_old_foil = .data$Foil,
      fm_ratio = .data$Lure - .data$Foil,
      fm_dprime = stats::qnorm(.data$Lure) - stats::qnorm(.data$Foil)
    )
  is_normal <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT >= 100, .data$ACC, 0L)) %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5))
  cbind(pc_all, fm, is_normal)
}
