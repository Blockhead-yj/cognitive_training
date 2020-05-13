context("`bart` test")
library(dataprocr2)
test_that("`bart` should deal with normal and abnormal data", {
  expect_equal(
    bart(
      jsonlite::read_json("data_bart.json", simplifyVector = TRUE)
    )$mean_hits, 5
  )
  expect_equal(
    bart(
      jsonlite::read_json("data_bart.json", simplifyVector = TRUE)
    )$mean_hits_raw, 5.2333, tolerance = 1e-4
  )
  expect_equal(
    bart(
      jsonlite::read_json("data_bart.json", simplifyVector = TRUE)
    )$is_normal, TRUE
  )
  expect_equal(is.na(bart(data.frame())$mean_hits), TRUE)
  expect_equal(is.na(bart(data.frame())$mean_hits_raw), TRUE)
  expect_equal(bart(data.frame())$is_normal, FALSE)
})
