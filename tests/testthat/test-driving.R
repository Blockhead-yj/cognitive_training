context("`driving` test")
library(dataprocr2)
test_that("`driving` should deal with normal and abnormal data", {
  result_names <- c("still_ratio", "is_normal")
  # Testing on normal data
  expect_silent(
    result_normal <- driving(
      jsonlite::read_json("data_driving.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$still_ratio, 0.515, tolerance = 0.001)
  expect_true(result_normal$is_normal)
  # Testing on abnormal data
  expect_warning(
    result_abnormal <- driving(data.frame()),
    "`YellowDur`, `StillDur` and `StillLight` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$still_ratio))
  expect_false(result_abnormal$is_normal)
})
