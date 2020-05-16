context("`count` test")
library(dataprocr2)
test_that("`count` should deal with normal and abnormal data", {
  # testing on Attention Search game
  expect_silent(
    result_attsearch <- count(
      jsonlite::read_json("data_count/data_attsearch.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_attsearch, c("count_correct", "is_normal"))
  expect_equal(result_attsearch$count_correct, 53)
  expect_true(result_attsearch$is_normal)
  # testing on Five Point Test game
  expect_silent(
    result_fpt <- count(
      jsonlite::read_json("data_count/data_fpt.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_fpt, c("count_correct", "is_normal"))
  expect_equal(result_fpt$count_correct, 49)
  expect_true(result_fpt$is_normal)
  # testing on Number Sets game
  expect_silent(
    result_numsets <- count(
      jsonlite::read_json("data_count/data_numsets.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_numsets, c("count_correct", "is_normal"))
  expect_equal(result_numsets$count_correct, 85)
  expect_true(result_numsets$is_normal)
  # testing on JLO game
  expect_silent(
    result_jlo <- count(
      jsonlite::read_json("data_count/data_jlo.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_jlo, c("count_correct", "is_normal"))
  expect_equal(result_jlo$count_correct, 13)
  expect_true(result_jlo$is_normal)
  # testing on Calc game
  expect_silent(
    result_calc <- count(
      jsonlite::read_json("data_count/data_calc.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calc, c("count_correct", "is_normal"))
  expect_equal(result_calc$count_correct, 24)
  expect_true(result_calc$is_normal)
  # testing on CalcJr game
  expect_silent(
    result_calcjr <- count(
      jsonlite::read_json("data_count/data_calcjr.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calcjr, c("count_correct", "is_normal"))
  expect_equal(result_calcjr$count_correct, 70)
  expect_true(result_calcjr$is_normal)
  # testing on CalcMed game
  expect_silent(
    result_calcmed <- count(
      jsonlite::read_json("data_count/data_calcmed.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_calcmed, c("count_correct", "is_normal"))
  expect_equal(result_calcmed$count_correct, 115)
  expect_true(result_calcmed$is_normal)
  # testing on Lexic game
  expect_silent(
    result_lexic <- count(
      jsonlite::read_json("data_count/data_lexic.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_lexic, c("count_correct", "is_normal"))
  expect_equal(result_lexic$count_correct, 86)
  expect_true(result_lexic$is_normal)
  # testing on Orthography game
  expect_silent(
    result_ortho <- count(
      jsonlite::read_json("data_count/data_ortho.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_ortho, c("count_correct", "is_normal"))
  expect_equal(result_ortho$count_correct, 88)
  expect_true(result_ortho$is_normal)
  # testing on Pinyin game
  expect_silent(
    result_pinyin <- count(
      jsonlite::read_json("data_count/data_pinyin.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_pinyin, c("count_correct", "is_normal"))
  expect_equal(result_pinyin$count_correct, 76)
  expect_true(result_pinyin$is_normal)
  # testing on Semanteme game
  expect_silent(
    result_seman <- count(
      jsonlite::read_json("data_count/data_seman.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_seman, c("count_correct", "is_normal"))
  expect_equal(result_seman$count_correct, 73)
  expect_true(result_seman$is_normal)
  # testing on Symbol game
  expect_silent(
    result_symbol <- count(
      jsonlite::read_json("data_count/data_symbol.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_symbol, c("count_correct", "is_normal"))
  expect_equal(result_symbol$count_correct, 141)
  expect_true(result_symbol$is_normal)
  # testing on Tone game
  expect_silent(
    result_tone <- count(
      jsonlite::read_json("data_count/data_tone.json", simplifyVector = TRUE)
    )
  )
  expect_named(result_tone, c("count_correct", "is_normal"))
  expect_equal(result_tone$count_correct, 56)
  expect_true(result_tone$is_normal)
  # testing on abnormal data input
  expect_warning(
    result_abnormal <- count(data.frame()),
    "Accuracy related variable, i.e., `ACC` or `Repetition`, is required."
  )
  expect_named(result_abnormal, c("count_correct", "is_normal"))
  expect_true(is.na(result_abnormal$count_correct))
  expect_false(result_abnormal$is_normal)
})
