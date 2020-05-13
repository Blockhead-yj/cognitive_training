context("`count` test")
library(dataprocr2)
test_that("`count` should deal with normal and abnormal data", {
  # testing on Attention Search game
  expect_silent(
    result_attsearch <- count(
      jsonlite::read_json("data_count/data_attsearch.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_attsearch, c("count_correct", "is_normal"))
  expect_equal(result_attsearch$count_correct, 53)
  expect_true(result_attsearch$is_normal)
  # testing on Five Point Test game
  expect_silent(
    result_fpt <- count(
      jsonlite::read_json("data_count/data_fpt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_fpt, c("count_correct", "is_normal"))
  expect_equal(result_fpt$count_correct, 49)
  expect_true(result_fpt$is_normal)
  # testing on Number Sets game
  expect_silent(
    result_numsets <- count(
      jsonlite::read_json("data_count/data_numsets.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_numsets, c("count_correct", "is_normal"))
  expect_equal(result_numsets$count_correct, 85)
  expect_true(result_numsets$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- count(data.frame()),
    "Accuracy related variable, i.e., `ACC` or `Repetition`, is required."
  )
  expect_named(result_abnormal, c("count_correct", "is_normal"))
  expect_true(is.na(result_abnormal$count_correct))
  expect_false(result_abnormal$is_normal)
})
