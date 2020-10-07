#' Test on normal data input
#'
#' The normal data from a given file will be imported and then passed into the
#' function being tested. The result will be compared with that imported from a
#' given result file based on a specified tolerance. This comparison does take
#' attributes into consideration.
test_normal <- function(test_fun, data_file, result_file, ..., tolerance = 1e-3) {
  expect_silent(
    result <- test_fun(
      jsonlite::read_json(data_file, simplifyVector = TRUE),
      ...
    )
  )
  expect_result <- jsonlite::read_json(result_file, simplifyVector = TRUE)
  expect_equivalent(result, expect_result, tolerance = tolerance)
}
#' Test on abnormal data input
#'
#' The abnormal data input used here is just an empty \code{data.frame}. The
#' expected result of call will have all \code{NA} values but one
#' \code{is_normal} variable with value of \code{FALSE}.
test_abnormal <- function(test_fun, result_file, warn_msg, ...) {
  expect_warning(result <- test_fun(data.frame(), ...), warn_msg)
  expect_result <- jsonlite::read_json(result_file, simplifyVector = TRUE)
  expect_named(result, names(expect_result))
  for (index in names(expect_result)) {
    if (index != "is_normal") {
      expect_true(is.na(result[[index]]))
    } else {
      expect_false(result[[index]])
    }
  }
}
