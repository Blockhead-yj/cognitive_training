#' Calculates index scores for Nonsymbolic Number Comparison game.
#'
#' Several values including percentage of correct responses (pc), mean reaction
#' time (mrt), weber fraction (w).
#'
#' @param data Raw data of class \code{data.frame}.
#' @param ... Other input argument for future expansion.
#' @return A \code{data.frame} contains following values:
#' \describe{
#'   \item{pc}{Percentage of correct responses.}
#'   \item{mrt}{Mean reaction time.}
#'   \item{w}{Weber fraction.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' }
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
nsymncmp <- function(data, ...) {
  if (!all(utils::hasName(data, c("BigSetCount", "SmallSetCount", "RT", "ACC")))) {
    warning("`BigSetCount`, `SmallSetCount`, `RT` and `ACC` variables are required.")
    return(
      data.frame(
        pc = NA_real_,
        mrt = NA_real_,
        w = NA_real_,
        is_normal = FALSE
      )
    )
  }
  data_adj <- data %>%
    dplyr::mutate(acc_adj = dplyr::if_else(.data$RT <= 100, 0L, .data$ACC)) %>%
    dplyr::rename(b = .data$BigSetCount, s = .data$SmallSetCount)
  basic <- data_adj %>%
    dplyr::summarise(
      pc = mean(.data$acc_adj == 1),
      mrt = mean(.data$RT[.data$acc_adj == 1])
    )
  fit_errproof <- purrr::possibly(
    ~ stats::nls(
      acc_adj ~ 1 - pnorm(0, b - s, w * sqrt(b ^ 2 + s ^ 2)),
      ., start = list(w = 0.5)
    ) %>%
      stats::coef(),
    otherwise = NA_real_
  )
  weber_fraction <- data.frame(w = fit_errproof(data_adj))
  is_normal <- data_adj %>%
    dplyr::summarise(n = dplyr::n(), count_correct = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(is_normal = .data$n > stats::qbinom(0.95, .data$n, 0.5))
  cbind(basic, weber_fraction, is_normal)
}
