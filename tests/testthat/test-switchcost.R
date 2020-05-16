context("`switchcost` test")
library(dataprocr2)
test_that("`switchcost` should deal with normal and abnormal data", {
  # testing on normal data
  expect_silent(
    result_normal <- switchcost(
      jsonlite::read_json("data_switchcost/data_cardsort.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("count_correct", "switch_cost_gen", "switch_cost_spe", "is_normal"))
  expect_equal(result_normal$count_correct, 59)
  expect_equal(result_normal$switch_cost_gen, 132.332, tolerance = 0.001)
  expect_equal(result_normal$switch_cost_spe, 116.572, tolerance = 0.001)
  expect_true(result_normal$is_normal)
  # testing on normal data of older version
  expect_silent(
    result_normal_old <- switchcost(
      jsonlite::read_json("data_switchcost/data_cardsort_old.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal_old, c("count_correct", "switch_cost_gen", "switch_cost_spe", "is_normal"))
  expect_equal(result_normal_old$count_correct, 53)
  expect_equal(result_normal_old$switch_cost_gen, 128.148, tolerance = 0.001)
  expect_equal(result_normal_old$switch_cost_spe, 136.552, tolerance = 0.001)
  expect_true(result_normal_old$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- switchcost(data.frame()),
    "`Type`, `ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, c("count_correct", "switch_cost_gen", "switch_cost_spe", "is_normal"))
  expect_true(is.na(result_abnormal$count_correct))
  expect_true(is.na(result_abnormal$switch_cost_gen))
  expect_true(is.na(result_abnormal$switch_cost_spe))
  expect_false(result_abnormal$is_normal)
})
