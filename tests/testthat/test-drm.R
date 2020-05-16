context("`drm` test")
library(dataprocr2)
test_that("`drm` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- drm(
      jsonlite::read_json("data_drm.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("fm_ratio", "fm_dprime", "is_normal"))
  expect_equal(result_normal$fm_ratio, 0.21875, tolerance = 1e-5)
  expect_equal(result_normal$fm_dprime, 0.9550, tolerance = 1e-4)
  expect_true(result_normal$is_normal)
  expect_warning(
    result_abnormal <- drm(data.frame()),
    "`Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, c("fm_ratio", "fm_dprime", "is_normal"))
  expect_true(is.na(result_abnormal$fm_ratio))
  expect_true(is.na(result_abnormal$fm_dprime))
  expect_false(result_abnormal$is_normal)
})
