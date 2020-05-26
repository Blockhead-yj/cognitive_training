context("`cpt` test")
library(dataprocr2)
test_that("`cpt` should deal with normal and abnormal data", {
  result_names <- c(
    "dprime", "c",
    "hits", "commissions", "omissions", "count_error",
    "mrt", "rtsd", "is_normal"
  )
  # testing on normal data
  expect_silent(
    result_normal <- cpt(
      jsonlite::read_json("data_cpt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$dprime, 4.334, tolerance = 1e-3)
  expect_equal(result_normal$c, 0.435, tolerance = 1e-3)
  expect_equal(result_normal$hits, 12)
  expect_equal(result_normal$commissions, 0)
  expect_equal(result_normal$omissions, 0)
  expect_equal(result_normal$count_error, 0)
  expect_equal(result_normal$mrt, 280.5, tolerance = 1e-1)
  expect_equal(result_normal$rtsd, 86.78552, tolerance = 1e-5)
  expect_true(result_normal$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- cpt(data.frame()),
    "`Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$dprime))
  expect_true(is.na(result_abnormal$c))
  expect_true(is.na(result_abnormal$hits))
  expect_true(is.na(result_abnormal$commissions))
  expect_true(is.na(result_abnormal$omissions))
  expect_true(is.na(result_abnormal$count_error))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$rtsd))
  expect_false(result_abnormal$is_normal)
})
