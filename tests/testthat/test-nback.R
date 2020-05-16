context("`nback` test")
library(dataprocr2)
test_that("`nback` should deal with normal and abnormal data", {
  # testing on Nback1 data
  expect_silent(
    result_nback1 <- nback(
      jsonlite::read_json("data_nback/data_nback1.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nback1, c("pc", "mrt", "dprime", "c", "is_normal"))
  expect_equal(result_nback1$pc, 0.875, tolerance = 0.001)
  expect_equal(result_nback1$mrt, 556.937, tolerance = 0.001)
  expect_equal(result_nback1$dprime, 2.306, tolerance = 0.001)
  expect_equal(result_nback1$c, 0.068, tolerance = 0.001)
  expect_true(result_nback1$is_normal)
  # testing on Nback2 data
  expect_silent(
    result_nback2 <- nback(
      jsonlite::read_json("data_nback/data_nback2.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nback2, c("pc", "mrt", "dprime", "c", "is_normal"))
  expect_equal(result_nback2$pc, 0.625, tolerance = 0.001)
  expect_equal(result_nback2$mrt, 287.889, tolerance = 0.001)
  expect_equal(result_nback2$dprime, 0.648, tolerance = 0.001)
  expect_equal(result_nback2$c, -0.184, tolerance = 0.001)
  expect_true(result_nback2$is_normal)
  # testing on Nback3 data
  expect_silent(
    result_nback3 <- nback(
      jsonlite::read_json("data_nback/data_nback3.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nback3, c("pc", "mrt", "dprime", "c", "is_normal"))
  expect_equal(result_nback3$pc, 0.75, tolerance = 0.01)
  expect_equal(result_nback3$mrt, 607.130, tolerance = 0.001)
  expect_equal(result_nback3$dprime, 1.370, tolerance = 0.001)
  expect_equal(result_nback3$c, -0.177, tolerance = 0.001)
  expect_true(result_nback3$is_normal)
  # testing on Nback4 data
  expect_silent(
    result_nback4 <- nback(
      jsonlite::read_json("data_nback/data_nback4.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_nback4, c("pc", "mrt", "dprime", "c", "is_normal"))
  expect_equal(result_nback4$pc, 0.681, tolerance = 0.01)
  expect_equal(result_nback4$mrt, 940.776, tolerance = 0.001)
  expect_equal(result_nback4$dprime, 0.945, tolerance = 0.001)
  expect_equal(result_nback4$c, -0.117, tolerance = 0.001)
  expect_true(result_nback4$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- nback(data.frame()),
    "`Type`, `RT` and `ACC` variables are required."
  )
  expect_named(result_abnormal, c("pc", "mrt", "dprime", "c", "is_normal"))
  expect_true(is.na(result_abnormal$pc))
  expect_true(is.na(result_abnormal$mrt))
  expect_true(is.na(result_abnormal$dprime))
  expect_true(is.na(result_abnormal$c))
  expect_false(result_abnormal$is_normal)
})
