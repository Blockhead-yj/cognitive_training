context("`countcorrect` test")
library(dataprocr2)
test_that(
  "`countcorrect` should work on all the sample data", {
    sample_files <- list.files("data/countcorrect", "sample", full.names = TRUE)
    for (sample_file in sample_files) {
      sample_label <- stringr::str_extract(sample_file, r"((?<=_)\w+(?=\.))")
      result_file <- file.path(
        dirname(sample_file),
        stringr::str_c("result_", sample_label, ".json")
      )
      test_normal(
        countcorrect,
        data_file = sample_file,
        result_file = result_file
      )
    }
  }
)
test_that(
  "`countcorrect` should output result with 'missing' values if input data is corrupted",
  test_abnormal(
    countcorrect,
    result_file = "data/countcorrect/result_assoclang_new.json",
    warn_msg = "One and only one accuracy related variable, i.e., `ACC`, `Repetition` or `Correctness`, is required."
  )
)
