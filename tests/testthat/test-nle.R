context("`nle` test")
library(dataprocr2)
test_that("`nle` should deal with normal and abnormal data", {
  # testing on NLEMed game
  expect_silent(
    result_nlemed <- nle(
      jsonlite::read_json("data_nle/data_nlemed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nlemed, c("mean_err", "is_normal"))
  expect_equal(result_nlemed$mean_err, 4.318, tolerance = 0.001)
  expect_true(result_nlemed$is_normal)
  # testing on NLEJr game
  expect_silent(
    result_nlejr <- nle(
      jsonlite::read_json("data_nle/data_nlejr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nlejr, c("mean_err", "is_normal"))
  expect_equal(result_nlejr$mean_err, 0.625, tolerance = 0.001)
  expect_true(result_nlejr$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- nle(data.frame()),
    "`Number` and `Resp` variables are required."
  )
  expect_named(result_abnormal, c("mean_err", "is_normal"))
  expect_true(is.na(result_abnormal$mean_err))
  expect_false(result_abnormal$is_normal)
})
