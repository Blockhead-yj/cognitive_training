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
    # values of NA (any mode) and NaN are treated as the same
    if (is.na(cur_exp)) {
      expect_true(is.na(cur_val))
      next
    }
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
#' The abnormal data input used here is just an empty `data.frame`. The expected
#' result of call will have all `NA` values but one `is_normal` variable with
#' value of `FALSE`.
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
#' Perform regular batch tests for a function
#'
#' Regularly, a test on the sample data files and a test with empty `data.frame`
#' input will be performed. This function does all this in a batch.
test_batch <- function(test_fun, ...,
                       warn_msg = "At least one of the required variables are missing.") {
  test_fun_str <- as_name(enquo(test_fun))
  test_that(
    stringr::str_glue("`{test_fun_str}` should work on all the sample data"), {
      sample_files <- list.files(
        file.path("data", test_fun_str),
        "sample", full.names = TRUE
      )
      for (sample_file in sample_files) {
        sample_label <- stringr::str_extract(sample_file, r"((?<=_)\w+(?=\.))")
        result_file <- file.path(
          dirname(sample_file),
          stringr::str_c("result_", sample_label, ".json")
        )
        test_normal(
          test_fun,
          data_file = sample_file,
          result_file = result_file,
          ...
        )
      }
    }
  )
  test_that(
    stringr::str_glue(
      "`{test_fun_str}` ",
      "should output result with 'missing' values if input data is corrupted"
    ),
    test_abnormal(
      test_fun,
      result_file = list.files(
        file.path("data", test_fun_str),
        "result", full.names = TRUE
      )[[1]],
      warn_msg = warn_msg,
      ...
    )
  )
}
