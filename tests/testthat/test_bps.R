context("`bps` test")
library(dataprocr2)
test_that("`bps` should deal with normal and abnormal data", {
  expect_warning(
    result_normal <- bps(
      jsonlite::read_json("data_bps.json", simplifyVector = TRUE)
    ), NA
  )
  expect_equal(result_normal$bps_score, 0.25, tolerance = 1e-2)
  expect_equal(result_normal$is_normal, TRUE)
  expect_warning(
    result_abnormal <- bps(data.frame()),
    "`Phase`, `Resp`, `Type`, `RT` and `ACC` variables are required."
  )
  expect_equal(is.na(result_abnormal$bps_score), TRUE)
  expect_equal(result_abnormal$is_normal, FALSE)
})
