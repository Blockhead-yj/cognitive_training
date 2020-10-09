context("`congeff` test")
library(dataprocr2)
test_that(
  "`congeff` should work on all the sample data", {
    sample_files <- list.files("data/congeff", "sample", full.names = TRUE)
    for (sample_file in sample_files) {
      sample_label <- stringr::str_extract(sample_file, r"((?<=_)\w+(?=\.))")
      result_file <- file.path(
        dirname(sample_file),
        stringr::str_c("result_", sample_label, ".json")
      )
      test_normal(
        congeff,
        data_file = sample_file,
        result_file = result_file
      )
    }
  }
)
test_that(
  "`congeff` should output result with 'missing' values if input data is corrupted",
  test_abnormal(
    congeff,
    result_file = "data/congeff/result_flanker.json",
    warn_msg = "`Type`, `ACC` and `RT` variables are required."
  )
)
