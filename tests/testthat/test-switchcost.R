context("`switchcost` test")
library(dataprocr2)
test_that(
  "`switchcost` should work on all the sample data", {
    sample_files <- list.files("data/switchcost", "sample", full.names = TRUE)
    for (sample_file in sample_files) {
      sample_label <- stringr::str_extract(sample_file, r"((?<=_)\w+(?=\.))")
      result_file <- file.path(
        dirname(sample_file),
        stringr::str_c("result_", sample_label, ".json")
      )
      test_normal(
        switchcost,
        data_file = sample_file,
        result_file = result_file
      )
    }
  }
)
test_that(
  "`switchcost` should output result with 'missing' values if input data is corrupted",
  test_abnormal(
    switchcost,
    result_file = "data/switchcost/result_cardsort.json",
    warn_msg = "`Type`, `ACC` and `RT` variables are required."
  )
)
