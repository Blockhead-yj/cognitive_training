context("`crt` test")
library(dataprocr2)
test_that("`crt` should deal with normal and abnormal data", {
  expect_equal(
    crt(
      jsonlite::read_json("data_crt.json", simplifyVector = TRUE)
    )$mrt, 326.494, tolerance = 1e-3
  )
  expect_equal(
    crt(
      jsonlite::read_json("data_crt.json", simplifyVector = TRUE)
    )$count_correct, 334
  )
  expect_equal(is.na(crt(data.frame())$mrt), TRUE)
  expect_equal(is.na(crt(data.frame())$count_correct), TRUE)
})
