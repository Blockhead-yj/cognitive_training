context("`countcorrect` test")
library(dataprocr2)
test_that("`countcorrect` should deal with normal and abnormal data", {
  # testing on Attention Search game
  expect_silent(
    result_attsearch <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_attsearch.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_attsearch, c("nc", "is_normal"))
  expect_equal(result_attsearch$nc, 53)
  expect_true(result_attsearch$is_normal)
  # testing on Five Point Test game
  expect_silent(
    result_fpt <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_fpt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_fpt, c("nc", "is_normal"))
  expect_equal(result_fpt$nc, 49)
  expect_true(result_fpt$is_normal)
  # testing on Number Sets game
  expect_silent(
    result_numsets <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_numsets.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_numsets, c("nc", "is_normal"))
  expect_equal(result_numsets$nc, 85)
  expect_true(result_numsets$is_normal)
  # testing on Calc game
  expect_silent(
    result_calc <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_calc.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calc, c("nc", "is_normal"))
  expect_equal(result_calc$nc, 24)
  expect_true(result_calc$is_normal)
  # testing on CalcJr game
  expect_silent(
    result_calcjr <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_calcjr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calcjr, c("nc", "is_normal"))
  expect_equal(result_calcjr$nc, 70)
  expect_true(result_calcjr$is_normal)
  # testing on CalcMed game
  expect_silent(
    result_calcmed <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_calcmed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calcmed, c("nc", "is_normal"))
  expect_equal(result_calcmed$nc, 115)
  expect_true(result_calcmed$is_normal)
  # testing on Lexic game
  expect_silent(
    result_lexic <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_lexic.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_lexic, c("nc", "is_normal"))
  expect_equal(result_lexic$nc, 86)
  expect_true(result_lexic$is_normal)
  # testing on Orthography game
  expect_silent(
    result_ortho <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_ortho.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_ortho, c("nc", "is_normal"))
  expect_equal(result_ortho$nc, 88)
  expect_true(result_ortho$is_normal)
  # testing on Pinyin game
  expect_silent(
    result_pinyin <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_pinyin.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_pinyin, c("nc", "is_normal"))
  expect_equal(result_pinyin$nc, 76)
  expect_true(result_pinyin$is_normal)
  # testing on Semanteme game
  expect_silent(
    result_seman <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_seman.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_seman, c("nc", "is_normal"))
  expect_equal(result_seman$nc, 73)
  expect_true(result_seman$is_normal)
  # testing on Symbol game
  expect_silent(
    result_symbol <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_symbol.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_symbol, c("nc", "is_normal"))
  expect_equal(result_symbol$nc, 141)
  expect_true(result_symbol$is_normal)
  # testing on Tone game
  expect_silent(
    result_tone <- countcorrect(
      jsonlite::read_json("data_countcorrect/data_tone.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_tone, c("nc", "is_normal"))
  expect_equal(result_tone$nc, 56)
  expect_true(result_tone$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- countcorrect(data.frame()),
    "Accuracy related variable, i.e., `ACC` or `Repetition`, is required."
  )
  expect_named(result_abnormal, c("nc", "is_normal"))
  expect_true(is.na(result_abnormal$nc))
  expect_false(result_abnormal$is_normal)
})
