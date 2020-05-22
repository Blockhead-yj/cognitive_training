context("`jlo` test")
library(dataprocr2)
test_that("`jlo` should deal with normal and abnormal data", {
  result_names <- c("count_correct", "sum_error", "is_normal")
  # Testing on normal data
  expect_silent(
    result_normal <- jlo(
      jsonlite::read_json("data_jlo.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$count_correct, 13)
  expect_equal(result_normal$sum_error, 174)
  expect_true(result_normal$is_normal)
  # Testing on abnormal data
  expect_warning(
    result_abnormal <- jlo(data.frame()),
    "`Angle`, `Resp` and `ACC` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$count_correct))
  expect_true(is.na(result_abnormal$sum_error))
  expect_false(result_abnormal$is_normal)
})
