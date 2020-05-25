context("`conflict` test")
library(dataprocr2)
test_that("`conflict` should deal with normal and abnormal data", {
  result_names <- c(
    "count_correct",
    "mrt_inc", "mrt_con", "cong_eff_rt",
    "pc_inc", "pc_con", "cong_eff_pc",
    "is_normal"
  )
  # testing on normal data
  expect_silent(
    result_normal <- conflict(
      jsonlite::read_json("data_conflict/data_flanker.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$count_correct, 65)
  expect_equal(result_normal$mrt_inc, 1224.5, tolerance = 0.1)
  expect_equal(result_normal$mrt_con, 1146.2, tolerance = 0.1)
  expect_equal(result_normal$cong_eff_rt, 78.31818, tolerance = 1e-5)
  expect_equal(result_normal$pc_inc, 1)
  expect_equal(result_normal$pc_con, 0.97, tolerance = 0.01)
  expect_equal(result_normal$cong_eff_pc, -0.02941, tolerance = 1e-5)
  expect_true(result_normal$is_normal)
  # testing on normal of older version
  expect_silent(
    result_normal2 <- conflict(
      jsonlite::read_json("data_conflict/data_flanker2.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal2, result_names)
  expect_equal(result_normal2$count_correct, 79)
  expect_equal(result_normal2$mrt_inc, 739.92, tolerance = 0.01)
  expect_equal(result_normal2$mrt_con, 727.25, tolerance = 0.01)
  expect_equal(result_normal2$cong_eff_rt, 12.67308, tolerance = 1e-5)
  expect_equal(result_normal2$pc_inc, 0.975, tolerance = 0.001)
  expect_equal(result_normal2$pc_con, 1)
  expect_equal(result_normal2$cong_eff_pc, 0.025, tolerance = 0.001)
  expect_true(result_normal2$is_normal)
  expect_warning(
    result_abnormal <- conflict(data.frame()),
    "`Type`, `ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$count_correct))
  expect_true(is.na(result_abnormal$mrt_inc))
  expect_true(is.na(result_abnormal$mrt_con))
  expect_true(is.na(result_abnormal$cong_eff_rt))
  expect_true(is.na(result_abnormal$pc_inc))
  expect_true(is.na(result_abnormal$pc_con))
  expect_true(is.na(result_abnormal$cong_eff_pc))
  expect_false(result_abnormal$is_normal)
})
