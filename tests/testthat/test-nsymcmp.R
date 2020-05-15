context("`nsymncmp` test")
library(dataprocr2)
test_that("`nsymncmp` should deal with normal and abnormal data", {
  # testing on normal data
  expect_silent(
    result_nsymncmp <- nsymncmp(
      jsonlite::read_json("data_nsymncmp.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nsymncmp, c("pc", "mrt", "w", "is_normal"))
  expect_equal(result_nsymncmp$pc, 0.75, tolerance = 0.01)
  expect_equal(result_nsymncmp$mrt, 1313.683, tolerance = 0.001)
  expect_equal(result_nsymncmp$w, 0.35, tolerance = 0.01)
  expect_true(result_nsymncmp$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- nsymncmp(data.frame()),
    "`BigSetCount`, `SmallSetCount`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, c("pc", "mrt", "w", "is_normal"))
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$w))
  expect_false(result_abnormal$is_normal)
})
