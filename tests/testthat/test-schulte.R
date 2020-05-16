context("`schulte` test")
library(dataprocr2)
test_that("`schulte` should deal with normal and abnormal data", {
  # testing on schulteJr game
  expect_silent(
    result_schultejr <- schulte(
      jsonlite::read_json("data_schulte/data_schultejr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_schultejr, c("net_cor", "is_normal"))
  expect_equal(result_schultejr$net_cor, 49)
  expect_true(result_schultejr$is_normal)
  # testing on schulteMed game
  expect_silent(
    result_schultemed <- schulte(
      jsonlite::read_json("data_schulte/data_schultemed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_schultemed, c("net_cor", "is_normal"))
  expect_equal(result_schultemed$net_cor, 33)
  expect_true(result_schultemed$is_normal)
  # testing on schulteMed game
  expect_silent(
    result_schulteadv <- schulte(
      jsonlite::read_json("data_schulte/data_schulteadv.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_schulteadv, c("net_cor", "is_normal"))
  expect_equal(result_schulteadv$net_cor, 71)
  expect_true(result_schulteadv$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- schulte(data.frame()),
    "`NCorrect` and `NError` variables are required."
  )
  expect_named(result_schultemed, c("net_cor", "is_normal"))
  expect_true(is.na(result_abnormal$net_cor))
  expect_false(result_abnormal$is_normal)
})
