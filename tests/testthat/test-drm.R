context("`drm` test")
library(dataprocr2)
test_that("`drm` should deal with normal and abnormal data", {
  result_names <- c("pc", "p_old_lure", "p_old_foil", "fm_ratio", "fm_dprime", "is_normal")
  # testing on normal data
  expect_silent(
    result_normal <- drm(
      jsonlite::read_json("data_drm.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$pc, 0.828, tolerance = 1e-3)
  expect_equal(result_normal$p_old_lure, 0.28125, tolerance = 1e-5)
  expect_equal(result_normal$p_old_foil, 0.0625, tolerance = 1e-4)
  expect_equal(result_normal$fm_ratio, 0.21875, tolerance = 1e-5)
  expect_equal(result_normal$fm_dprime, 0.9550, tolerance = 1e-4)
  expect_true(result_normal$is_normal)
  # testing on abnormal data
  expect_warning(
    result_abnormal <- drm(data.frame()),
    "`Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$p_old_lure))
  expect_true(is.na(result_abnormal$p_old_foil))
  expect_true(is.na(result_abnormal$fm_ratio))
  expect_true(is.na(result_abnormal$fm_dprime))
  expect_false(result_abnormal$is_normal)
})
