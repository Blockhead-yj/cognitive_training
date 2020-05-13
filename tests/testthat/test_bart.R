context("`bart` test")
library(dataprocr2)
test_that("`bart` should deal with normal and abnormal data", {
  expect_silent(
    result_normal <- bart(
      jsonlite::read_json("data_bart.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_normal, c("mean_hits", "mean_hits_raw", "is_normal"))
  expect_equal(result_normal$mean_hits, 5)
  expect_equal(result_normal$mean_hits_raw, 5.2333, tolerance = 1e-4)
  expect_true(result_normal$is_normal)
  expect_warning(
    result_abnormal <- bart(data.frame()),
    "`NHit` and `Feedback` variables are required."
  )
  expect_named(result_abnormal, c("mean_hits", "mean_hits_raw", "is_normal"))
  expect_true(is.na(result_abnormal$mean_hits))
  expect_true(is.na(result_abnormal$mean_hits_raw))
  expect_false(result_abnormal$is_normal)
})
