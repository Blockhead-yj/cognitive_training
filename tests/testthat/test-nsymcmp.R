context("`nsymncmp` test")
library(dataprocr2)
test_that("`nsymncmp` should deal with normal and abnormal data", {
  # testing on normal data
  expect_silent(
    result_nsymncmpmed <- nsymncmp(
      jsonlite::read_json("data_nsymncmp.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nsymncmpmed, c("pc", "mrt", "w", "is_normal"))
  expect_equal(result_nsymncmpmed$pc, 0.75, tolerance = 0.01)
  expect_equal(result_nsymncmpmed$mrt, 1313.683, tolerance = 0.001)
  expect_equal(result_nsymncmpmed$w, 0.35, tolerance = 0.01)
  expect_true(result_nsymncmpmed$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- nsymncmp(data.frame()),
    "`BigSetCount`, `SmallSetCount`, `RT` and `ACC` variables are required."
  )
  expect_named(result_nsymncmpmed, c("pc", "mrt", "w", "is_normal"))
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$w))
  expect_false(result_abnormal$is_normal)
})
