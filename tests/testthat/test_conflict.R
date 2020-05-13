context("`conflict` test")
library(dataprocr2)
test_that("`conflict` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- conflict(
      jsonlite::read_json("data_flanker.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("count_correct", "cong_eff_rt", "cong_eff_pc", "is_normal"))
  expect_equal(result_normal$count_correct, 65)
  expect_equal(result_normal$cong_eff_rt, 78.31818, tolerance = 1e-5)
  expect_equal(result_normal$cong_eff_pc, -0.02941, tolerance = 1e-5)
  expect_true(result_normal$is_normal)
  expect_silent(
    result_normal2 <- conflict(
      jsonlite::read_json("data_flanker2.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal2, c("count_correct", "cong_eff_rt", "cong_eff_pc", "is_normal"))
  expect_equal(result_normal2$count_correct, 79)
  expect_equal(result_normal2$cong_eff_rt, 12.67308, tolerance = 1e-5)
  expect_equal(result_normal2$cong_eff_pc, 0.025, tolerance = 1e-3)
  expect_true(result_normal2$is_normal)
  expect_warning(
    result_abnormal <- conflict(data.frame()),
    "`Type`, `ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, c("count_correct", "cong_eff_rt", "cong_eff_pc", "is_normal"))
  expect_true(is.na(result_abnormal$count_correct))
  expect_true(is.na(result_abnormal$cong_eff_rt))
  expect_true(is.na(result_abnormal$cong_eff_pc))
  expect_false(result_abnormal$is_normal)
})
