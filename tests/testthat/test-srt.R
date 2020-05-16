context("`srt` test")
library(dataprocr2)
test_that("`srt` should deal with normal and abnormal data", {
  # testing on normal data
  expect_silent(
    result_normal <- srt(
      jsonlite::read_json("data_srt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("mrt", "is_normal"))
  expect_equal(result_normal$mrt, 149.556, tolerance = 0.001)
  expect_true(result_normal$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- srt(data.frame()),
    "`RT` variable is required."
  )
  expect_named(result_abnormal, c("mrt", "is_normal"))
  expect_true(is.na(result_abnormal$mrt))
  expect_false(result_abnormal$is_normal)
})
