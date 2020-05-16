context("`locmem` test")
library(dataprocr2)
test_that("`locmem` should deal with normal and abnormal data", {
  # testing on LocMem game
  expect_silent(
    result_locmem <- locmem(
      jsonlite::read_json("data_locmem/data_locmem.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_locmem, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_locmem$mean_dist, 3.128, tolerance = 0.001)
  expect_equal(result_locmem$pc, 0.269, tolerance = 0.001)
  expect_true(result_locmem$is_normal)
  # testing on LocMemJr game
  expect_silent(
    result_locmemjr <- locmem(
      jsonlite::read_json("data_locmem/data_locmemjr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_locmemjr, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_locmemjr$mean_dist, 0.89, tolerance = 0.01)
  expect_equal(result_locmemjr$pc, 0.666, tolerance = 0.001)
  expect_true(result_locmemjr$is_normal)
  # testing on LocMemMed game
  expect_silent(
    result_locmemmed <- locmem(
      jsonlite::read_json("data_locmem/data_locmemmed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_locmemmed, c("mean_dist", "pc", "is_normal"))
  expect_equal(result_locmemmed$mean_dist, 0.704, tolerance = 0.001)
  expect_equal(result_locmemmed$pc, 0.690, tolerance = 0.001)
  expect_true(result_locmemmed$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- locmem(data.frame()),
    "`RespLocDist` variable is required."
  )
  expect_named(result_abnormal, c("mean_dist", "pc", "is_normal"))
  expect_true(is.na(result_abnormal$mean_dist))
  expect_true(is.na(result_abnormal$pc))
  expect_false(result_abnormal$is_normal)
})
