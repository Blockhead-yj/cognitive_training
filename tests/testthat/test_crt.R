context("`crt` test")
library(dataprocr2)
test_that("`crt` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- crt(
      jsonlite::read_json("data_crt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("mrt", "count_correct", "is_normal"))
  expect_equal(result_normal$mrt, 326.494, tolerance = 1e-3)
  expect_equal(result_normal$count_correct, 334)
  expect_true(result_normal$is_normal)
  expect_warning(
    result_abnormal <- crt(data.frame()),
    "`ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, c("mrt", "count_correct", "is_normal"))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$count_correct))
  expect_false(result_abnormal$is_normal)
})
