context("`crt` test")
library(dataprocr2)
test_that("`crt` should deal with normal and abnormal data", {
  expect_warning(
    result_normal <- crt(
      jsonlite::read_json("data_crt.json", simplifyVector = TRUE)
    ), NA
  )
  expect_equal(result_normal$mrt, 326.494, tolerance = 1e-3)
  expect_equal(result_normal$count_correct, 334)
  expect_equal(result_normal$is_normal, TRUE)
  expect_warning(
    result_abnormal <- crt(data.frame()),
    "`ACC` and `RT` variables are required."
  )
  expect_equal(is.na(result_abnormal$mrt), TRUE)
  expect_equal(is.na(result_abnormal$count_correct), TRUE)
  expect_equal(result_abnormal$is_normal, FALSE)
})
