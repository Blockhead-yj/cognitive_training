context("`switchcost` test")
library(dataprocr2)
test_that("`switchcost` should deal with normal and abnormal data", {
  result_names <- c(
    "nc", "count_pure", "count_mixed", "switch_cost_gen_count",
    "mrt_pure", "mrt_repeat", "mrt_switch",
    "switch_cost_gen_rt", "switch_cost_spe_rt",
    "is_normal"
  )
  # testing on normal data of older version
  expect_silent(
    result_old <- switchcost(
      jsonlite::read_json("data_switchcost/data_cardsort_old.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_old, result_names)
  expect_equal(result_old$nc, 53)
  expect_true(is.na(result_old$count_pure))
  expect_true(is.na(result_old$count_mixed))
  expect_true(is.na(result_old$switch_cost_gen_count))
  expect_equal(result_old$mrt_pure, 1099.2, tolerance = 0.1)
  expect_equal(result_old$mrt_repeat, 1227.348, tolerance = 0.001)
  expect_equal(result_old$mrt_switch, 1363.9, tolerance = 0.1)
  expect_equal(result_old$switch_cost_gen_rt, 128.148, tolerance = 0.001)
  expect_equal(result_old$switch_cost_spe_rt, 136.552, tolerance = 0.001)
  expect_true(result_old$is_normal)
  # testing on normal data of newer version with 5 blocks
  expect_silent(
    result_new5 <- switchcost(
      jsonlite::read_json("data_switchcost/data_cardsort5.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_new5, result_names)
  expect_equal(result_new5$nc, 47)
  expect_equal(result_new5$count_pure, 10)
  expect_equal(result_new5$count_mixed, 9)
  expect_equal(result_new5$switch_cost_gen_count, 1, tolerance = 0.1)
  expect_equal(result_new5$mrt_pure, 1087.375, tolerance = 0.001)
  expect_equal(result_new5$mrt_repeat, 2173.348, tolerance = 0.001)
  expect_equal(result_new5$mrt_switch, 3126.231, tolerance = 0.001)
  expect_equal(result_new5$switch_cost_gen_rt, 1086.08, tolerance = 0.01)
  expect_equal(result_new5$switch_cost_spe_rt, 952.776, tolerance = 0.001)
  expect_true(result_new5$is_normal)
  # testing on normal data of newer version with 6 blocks
  expect_silent(
    result_new6 <- switchcost(
      jsonlite::read_json("data_switchcost/data_cardsort6.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_new6, result_names)
  expect_equal(result_new6$nc, 60)
  expect_equal(result_new6$count_pure, 14)
  expect_equal(result_new6$count_mixed, 11.5)
  expect_equal(result_new6$switch_cost_gen_count, 2.5, tolerance = 0.1)
  expect_equal(result_new6$mrt_pure, 977.929, tolerance = 0.001)
  expect_equal(result_new6$mrt_repeat, 1197.3, tolerance = 0.1)
  expect_equal(result_new6$mrt_switch, 1281.818, tolerance = 0.001)
  expect_equal(result_new6$switch_cost_gen_rt, 219.371, tolerance = 0.001)
  expect_equal(result_new6$switch_cost_spe_rt, 84.518, tolerance = 0.001)
  expect_true(result_new6$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- switchcost(data.frame()),
    "`Block`, `Type`, `ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$nc))
  expect_true(is.na(result_abnormal$count_pure))
  expect_true(is.na(result_abnormal$count_mixed))
  expect_true(is.na(result_abnormal$switch_cost_gen_count))
  expect_true(is.na(result_abnormal$mrt_pure))
  expect_true(is.na(result_abnormal$mrt_repeat))
  expect_true(is.na(result_abnormal$mrt_switch))
  expect_true(is.na(result_abnormal$switch_cost_gen_rt))
  expect_true(is.na(result_abnormal$switch_cost_spe_rt))
  expect_false(result_abnormal$is_normal)
})
