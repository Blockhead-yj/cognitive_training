context("`symncmp` test")
library(dataprocr2)
test_that("`symncmp` should deal with normal and abnormal data", {
  # testing on normal data
  expect_silent(
    result_symncmp <- symncmp(
      jsonlite::read_json("data_symncmp.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_symncmp, c("pc", "mrt", "dist_eff", "dist_eff_adj", "is_normal"))
  expect_equal(result_symncmp$pc, 0.93, tolerance = 0.01)
  expect_equal(result_symncmp$mrt, 568.373, tolerance = 0.001)
  expect_equal(result_symncmp$dist_eff, 5.80, tolerance = 0.01)
  expect_equal(result_symncmp$dist_eff_adj, 0.0102, tolerance = 1e-4)
  expect_true(result_symncmp$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- symncmp(data.frame()),
    "`Small`, `Big`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, c("pc", "mrt", "dist_eff", "dist_eff_adj", "is_normal"))
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$dist_eff))
  expect_true(is.na(result_abnormal$dist_eff_adj))
  expect_false(result_abnormal$is_normal)
})
