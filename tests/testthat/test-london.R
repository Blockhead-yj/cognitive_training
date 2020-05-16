context("`london` test")
library(dataprocr2)
test_that("`london` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- london(
      jsonlite::read_json("data_london.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("tscore", "mean_level", "is_normal"))
  expect_equal(result_normal$tscore, 6490)
  expect_equal(result_normal$mean_level, 6.9, tolerance = 0.1)
  expect_true(result_normal$is_normal)
  expect_warning(
    result_abnormal <- london(data.frame()),
    "`LeveL`, `Score` and `Outcome` variables are required."
  )
  expect_named(result_abnormal, c("tscore", "mean_level", "is_normal"))
  expect_true(is.na(result_abnormal$tscore))
  expect_true(is.na(result_abnormal$mean_level))
  expect_false(result_abnormal$is_normal)
})
