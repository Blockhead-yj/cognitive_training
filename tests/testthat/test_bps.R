context("`bps` test")
library(dataprocr2)
test_that("`bps` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- bps(
      jsonlite::read_json("data_bps.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("bps_score", "is_normal"))
  expect_equal(result_normal$bps_score, 0.25, tolerance = 1e-2)
  expect_true(result_normal$is_normal)
  expect_warning(
    result_abnormal <- bps(data.frame()),
    "`Phase`, `Resp`, `Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, c("bps_score", "is_normal"))
  expect_true(is.na(result_abnormal$bps_score))
  expect_false(result_abnormal$is_normal)
})
