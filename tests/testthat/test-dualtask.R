context("`dualtask` test")
library(dataprocr2)
test_that(
  "`dualtask` should work on the given sample data",
  test_normal(
    test_fun = dualtask,
    data_file = "data/dualtask/sample.json",
    result_file = "data/dualtask/result_sample.json"
  )
)
test_that(
  "`dualtask` should output specif result if input data is corrupted",
  test_abnormal(
    test_fun = dualtask,
    result_file = "data/dualtask/result_sample.json",
    warn_msg = "`Side`, `StimType`, `ACC` and `RT` variables are required."
  )
)
