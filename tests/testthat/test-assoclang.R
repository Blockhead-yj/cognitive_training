context("`assoclang` test")
library(dataprocr2)
test_that("`assoclang` should deal with normal and abnormal data", {
  result_names <- c("nc", "pc", "is_normal")
  # Testing on normal data
  expect_silent(
    result_normal <- assoclang(
      jsonlite::read_json("data_assoclang.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, result_names)
  expect_equal(result_normal$nc, 17)
  expect_equal(result_normal$pc, 0.29, tolerance = 0.01)
  expect_true(result_normal$is_normal)
  # Testing on abnormal data
  expect_warning(
    result_abnormal <- assoclang(data.frame()),
    "`Correctness` variable is required."
  )
  expect_named(result_abnormal, result_names)
  expect_true(is.na(result_abnormal$nc))
  expect_true(is.na(result_abnormal$pc))
  expect_false(result_abnormal$is_normal)
})
