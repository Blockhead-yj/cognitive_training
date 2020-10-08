context("`jlo` test")
library(dataprocr2)
test_that("`jlo` should deal with normal and abnormal data", {
  result_names <- c("nc", "ne", "ne_ln", "ne_sqrt", "is_normal")
  # Testing on normal data
  expect_silent(
    result_normal <- jlo(
      jsonlite::read_json("data_jlo.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$nc, 13)
  expect_equal(result_normal$ne, 174)
  expect_equal(result_normal$ne_ln, 41.99, tolerance = 0.01)
  expect_equal(result_normal$ne_sqrt, 55.50, tolerance = 0.01)
  expect_true(result_normal$is_normal)
  # Testing on abnormal data
  expect_warning(
    result_abnormal <- jlo(data.frame()),
    "`Angle`, `Resp` and `ACC` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$nc))
  expect_true(is.na(result_abnormal$ne))
  expect_true(is.na(result_abnormal$ne_ln))
  expect_true(is.na(result_abnormal$ne_sqrt))
  expect_false(result_abnormal$is_normal)
})
