context("`multisense` test")
library(dataprocr2)
test_that("`multisense` should deal with normal and abnormal data", {
  result_names <- c("mrt_image", "mrt_sound", "mrt_mixed", "mrt_mixadv", "is_normal")
  # Testing on normal data
  expect_silent(
    result_normal <- multisense(
      jsonlite::read_json("data_multisense.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$mrt_image, 463.96, tolerance = 0.01)
  expect_equal(result_normal$mrt_sound, 433.5, tolerance = 0.1)
  expect_equal(result_normal$mrt_mixed, 446.54, tolerance = 0.01)
  expect_equal(result_normal$mrt_mixadv, 2.19, tolerance = 0.01)
  expect_true(result_normal$is_normal)
  # Testing on abnormal data
  expect_warning(
    result_abnormal <- multisense(data.frame()),
    "`Type` and `RT` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$mrt_image))
  expect_true(is.na(result_abnormal$mrt_sound))
  expect_true(is.na(result_abnormal$mrt_mixed))
  expect_true(is.na(result_abnormal$mrt_mixadv))
  expect_false(result_abnormal$is_normal)
})
