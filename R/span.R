#' Calculates index scores for Span related games
#'
#' Three indices are returned: maximal span, mean span and count of correct
#' responses.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A `data.frame` contains following values:
#'   \item{max_span}{Maximal span.}
#'   \item{mean_span}{Mean span.}
#'   \item{nc}{Count of correct responses.}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
span <- function(data, ...) {
  delim <- "-"
  len_var_opts <- c("NumTarget", "SLen")
  acc_var_opts <- c("Correctness", "AccLoc")
  len_var_idx <- utils::hasName(data, len_var_opts)
  acc_var_idx <- utils::hasName(data, acc_var_opts)
  if (!any(len_var_idx) | !utils::hasName(data, "Outcome")) {
    warning("Length related variable and `Outcome` variable are required.")
    return(
      data.frame(
        max_span = NA_real_,
        mean_span = NA_real_,
        nc = NA_real_,
        is_normal = FALSE
      )
    )
  }
  len_var <- len_var_opts[len_var_idx]
  acc_var <- acc_var_opts[acc_var_idx]
  if (!any(acc_var_idx)) {
    acc_var <- "Correctness"
    data <- data %>%
      dplyr::mutate(
        Correctness = purrr::map2_chr(
          .data$STIM, .data$Resp,
          ~ {
            stims <- unlist(strsplit(.x, delim))
            resps <- unlist(strsplit(.y, delim))
            len_diff <- length(stims) - length(resps)
            if (len_diff > 0) {
              resps <- c(resps, rep(-1, len_diff))
            }
            paste(as.numeric(stims == resps), collapse = delim)
          }
        )
      )
  }
  span_indices <- data %>%
    dplyr::mutate(max_span = max(!!sym(len_var))) %>%
    dplyr::group_by(!!sym(len_var), .data$max_span) %>%
    dplyr::summarise(pc = mean(.data$Outcome == 1)) %>%
    dplyr::group_by(.data$max_span) %>%
    dplyr::summarise(
      mean_span = min(!!rlang::sym(len_var)) - 0.5 + sum(.data$pc)
    )
  nc <- data %>%
    dplyr::pull(!!acc_var) %>%
    paste(collapse = delim) %>%
    strsplit(delim) %>%
    unlist() %>%
    as.numeric() %>%
    sum()
  cbind(span_indices, nc, is_normal = TRUE)
}
