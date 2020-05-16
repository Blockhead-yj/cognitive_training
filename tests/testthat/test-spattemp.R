context("`spattemp` test")
library(dataprocr2)
test_that("`spattemp` should deal with normal and abnormal data", {
  # testing on SpatTemp game
  expect_silent(
    result_spattemp <- spattemp(
      jsonlite::read_json("data_spattemp/data_spattemp.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_spattemp, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_spattemp$mean_dist, 0.896, tolerance = 0.001)
  expect_equal(result_spattemp$pc, 0.567, tolerance = 0.001)
  expect_true(result_spattemp$is_normal)
  # testing on SpatTempJr game
  expect_silent(
    result_spattempjr <- spattemp(
      jsonlite::read_json("data_spattemp/data_spattempjr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_spattempjr, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_spattempjr$mean_dist, 0.805, tolerance = 0.001)
  expect_equal(result_spattempjr$pc, 0.5, tolerance = 0.1)
  expect_true(result_spattempjr$is_normal)
  # testing on spattempMed game
  expect_silent(
    result_spattempmed <- spattemp(
      jsonlite::read_json("data_spattemp/data_spattempmed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_spattempmed, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_spattempmed$mean_dist, 0.515, tolerance = 0.001)
  expect_equal(result_spattempmed$pc, 0.5, tolerance = 0.1)
  expect_true(result_spattempmed$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- spattemp(data.frame()),
    "`RespAccOrder` and `RespLocDist` variables are required."
  )
  expect_named(result_abnormal, c("mean_dist", "pc", "is_normal"))
  expect_true(is.na(result_abnormal$mean_dist))
  expect_true(is.na(result_abnormal$pc))
  expect_false(result_abnormal$is_normal)
})
