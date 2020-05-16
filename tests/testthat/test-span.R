context("`span` test")
library(dataprocr2)
test_that("`span` should deal with normal and abnormal data", {
  # testing on BDS game
  expect_silent(
    result_bds <- span(
      jsonlite::read_json("data_span/data_bds.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_bds, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_bds$max_span, 9)
  expect_equal(result_bds$mean_span, 7.37, tolerance = 0.01)
  expect_equal(result_bds$count_correct, 19)
  expect_true(result_bds$is_normal)
  # testing on FDS game
  expect_silent(
    result_fds <- span(
      jsonlite::read_json("data_span/data_fds.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_fds, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_fds$max_span, 16)
  expect_equal(result_fds$mean_span, 16.5, tolerance = 0.1)
  expect_equal(result_fds$count_correct, 133)
  expect_true(result_fds$is_normal)
  # testing on FWS game
  expect_silent(
    result_fws <- span(
      jsonlite::read_json("data_span/data_fws.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_fws, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_fws$max_span, 16)
  expect_equal(result_fws$mean_span, 16.5, tolerance = 0.1)
  expect_equal(result_fws$count_correct, 0)
  expect_true(result_fws$is_normal)
  # testing on MOT game
  expect_silent(
    result_mot <- span(
      jsonlite::read_json("data_span/data_mot.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_mot, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_mot$max_span, 8)
  expect_equal(result_mot$mean_span, 6.83, tolerance = 0.01)
  expect_equal(result_mot$count_correct, 57)
  expect_true(result_mot$is_normal)
  # testing on MOT2 game
  expect_silent(
    result_mot2 <- span(
      jsonlite::read_json("data_span/data_mot2.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_mot2, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_mot2$max_span, 5)
  expect_equal(result_mot2$mean_span, 3.08, tolerance = 0.01)
  expect_equal(result_mot2$count_correct, 23)
  expect_true(result_mot2$is_normal)
  # testing on SSpan game
  expect_silent(
    result_sspan <- span(
      jsonlite::read_json("data_span/data_sspan.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_sspan, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_sspan$max_span, 7)
  expect_equal(result_sspan$mean_span, 5.17, tolerance = 0.01)
  expect_equal(result_sspan$count_correct, 49)
  expect_true(result_sspan$is_normal)
  # testing on SSTM game
  expect_silent(
    result_sstm <- span(
      jsonlite::read_json("data_span/data_sstm.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_sstm, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_sstm$max_span, 5)
  expect_equal(result_sstm$mean_span, 3.93, tolerance = 0.01)
  expect_equal(result_sstm$count_correct, 40)
  expect_true(result_sstm$is_normal)
  # testing on SSTMS game
  expect_silent(
    result_sstms <- span(
      jsonlite::read_json("data_span/data_sstms.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_sstms, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_equal(result_sstms$max_span, 8)
  expect_equal(result_sstms$mean_span, 6.23, tolerance = 0.01)
  expect_equal(result_sstms$count_correct, 73)
  expect_true(result_sstms$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- span(data.frame()),
    "Length related variable and `Outcome` variable are required."
  )
  expect_named(result_abnormal, c("max_span", "mean_span", "count_correct", "is_normal"))
  expect_true(is.na(result_abnormal$max_span))
  expect_true(is.na(result_abnormal$mean_span))
  expect_true(is.na(result_abnormal$count_correct))
  expect_false(result_abnormal$is_normal)
})
