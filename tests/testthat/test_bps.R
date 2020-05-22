context("`bps` test")
library(dataprocr2)
test_that("`bps` should deal with normal and abnormal data", {
  result_names <- c("pc", "p_sim_lure", "p_sim_foil", "p_sim_old", "bps_score", "is_normal")
  # testing on normal data
  expect_silent(
    result_normal <- bps(
      jsonlite::read_json("data_bps.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$pc, 0.59, tolerance = 0.01)
  expect_equal(result_normal$p_sim_lure, 0.425, tolerance = 0.001)
  expect_equal(result_normal$p_sim_foil, 0.175, tolerance = 0.001)
  expect_equal(result_normal$p_sim_old, 0.1, tolerance = 0.1)
  expect_equal(result_normal$bps_score, 0.25, tolerance = 0.01)
  expect_true(result_normal$is_normal)
  # testing on abnormal data
  expect_warning(
    result_abnormal <- bps(data.frame()),
    "`Phase`, `Resp`, `Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$p_sim_lure))
  expect_true(is.na(result_abnormal$p_sim_foil))
  expect_true(is.na(result_abnormal$p_sim_old))
  expect_true(is.na(result_abnormal$bps_score))
  expect_false(result_abnormal$is_normal)
})
