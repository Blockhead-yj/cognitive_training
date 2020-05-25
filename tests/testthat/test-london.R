context("`london` test")
library(dataprocr2)
test_that("`london` should deal with normal and abnormal data", {
  result_names <- c("tscore", "ratio_score", "mean_level", "is_normal")
  # testing on normal data
  expect_silent(
    result_normal <- london(
      jsonlite::read_json("data_london.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$tscore, 6490)
  expect_equal(result_normal$ratio_score, 0.469, tolerance = 0.001)
  expect_equal(result_normal$mean_level, 6.9, tolerance = 0.1)
  expect_true(result_normal$is_normal)
  # testing on normal data
  expect_warning(
    result_abnormal <- london(data.frame()),
    "`LeveL`, `Score` and `Outcome` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$tscore))
  expect_true(is.na(result_abnormal$ratio_score))
  expect_true(is.na(result_abnormal$mean_level))
  expect_false(result_abnormal$is_normal)
})
