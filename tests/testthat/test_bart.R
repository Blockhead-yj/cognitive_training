context("`bart` test")
library(dataprocr2)
test_that("`bart` should deal with normal and abnormal data", {
  expect_warning(
    result_normal <- bart(
      jsonlite::read_json("data_bart.json", simplifyVector = TRUE)
    ), NA
  )
  expect_equal(result_normal$mean_hits, 5)
  expect_equal(result_normal$mean_hits_raw, 5.2333, tolerance = 1e-4)
  expect_equal(result_normal$is_normal, TRUE)
  expect_warning(
    result_abnormal <- bart(data.frame()),
    "`NHit` and `Feedback` variables are required."
  )
  expect_equal(is.na(result_abnormal$mean_hits), TRUE)
  expect_equal(is.na(result_abnormal$mean_hits_raw), TRUE)
  expect_equal(result_abnormal$is_normal, FALSE)
})
