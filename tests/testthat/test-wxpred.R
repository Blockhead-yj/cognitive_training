context("`wxpred` test")
library(dataprocr2)
test_that("`wxpred` should deal with normal and abnormal data", {
  # testing on normal data
  test_normal(
    test_fun = wxpred,
    data_file = "data/wxpred/sample.json",
    result_file = "data/wxpred/result_sample.json"
  )
  # testing on abnormal data
  test_abnormal(
    test_fun = wxpred,
    result_file = "data/wxpred/result_sample.json",
    warn_msg = "`Block`, `ACC` and `RT` variables are required."
  )
})
