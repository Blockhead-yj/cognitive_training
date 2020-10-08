#' Test on normal data input
#'
#' The normal data sample from a given file will be imported and then passed
#' onto the function being tested. The result will be compared with the expected
#' result which is imported from a given result file. This comparison does take
#' attributes into consideration, and the tolerance is also based on the
#' precision of each value from the file.
test_normal <- function(test_fun, data_file, result_file, ...) {
  expect_silent(
    result <- test_fun(
      jsonlite::read_json(data_file, simplifyVector = TRUE),
      ...
    )
  )
  expect_result <- jsonlite::read_json(result_file, simplifyVector = TRUE)
  expect_named(result, names(expect_result), ignore.order = TRUE)
  for (index in names(expect_result)) {
    cur_val <- result[[index]]
    cur_exp <- expect_result[[index]]
    # the precision is set as the same as that in the expect result file
    tol_digit <- 0
    if (is.double(cur_exp)) {
      tol_digit <- as.character(cur_exp) %>%
        stringr::str_extract(r"((?<=\.)\d+)") %>%
        nchar()
    }
    expect_equal(cur_val, cur_exp, tolerance = 10 ^ (- tol_digit))
  }
}
#' Test on abnormal data input
#'
#' The abnormal data input used here is just an empty `data.frame`. The
#' expected result of call will have all `NA` values but one
#' `is_normal` variable with value of `FALSE`.
test_abnormal <- function(test_fun, result_file, warn_msg, ...) {
  expect_warning(result <- test_fun(data.frame(), ...), warn_msg)
  expect_result <- jsonlite::read_json(result_file, simplifyVector = TRUE)
  expect_named(result, names(expect_result), ignore.order = TRUE)
  for (index in names(expect_result)) {
    if (index != "is_normal") {
      expect_true(is.na(result[[index]]))
    } else {
      expect_false(result[[index]])
    }
  }
}
