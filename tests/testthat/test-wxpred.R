context("`wxpred` test")
library(dataprocr2)
test_that("`wxpred` should deal with normal and abnormal data", {
  result_names <- c("pc_b1", "pc_b2", "pc_b3", "pc_b4", "is_normal")
  # testing on normal data
  expect_silent(
    result_normal <- wxpred(
      jsonlite::read_json("data_wxpred.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$pc_b1, 0.48, tolerance = 0.01)
  expect_equal(result_normal$pc_b2, 0.72, tolerance = 0.01)
  expect_equal(result_normal$pc_b3, 0.52, tolerance = 0.01)
  expect_equal(result_normal$pc_b4, 0.6, tolerance = 0.1)
  expect_true(result_normal$is_normal)
  # testing on normal data
  expect_warning(
    result_abnormal <- wxpred(data.frame()),
    "`Block`, `ACC` and `RT` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$pc_b1))
  expect_true(is.na(result_abnormal$pc_b2))
  expect_true(is.na(result_abnormal$pc_b3))
  expect_true(is.na(result_abnormal$pc_b4))
  expect_false(result_abnormal$is_normal)
})
