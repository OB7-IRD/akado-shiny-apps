# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_sample_without_measure_inspector", {
  # Created from @doctest for `check_sample_without_measure_inspector`
  # Source file: R/function.R
  # Source line: 3799
  dataframe1 <- data.frame(samplespecies_id = c("1", "2"))
  dataframe2 <- data.frame(samplespeciesmeasure_id = c("1", "2"), samplespecies_id = c("1", "1"))
  expect_equal(check_sample_without_measure_inspector(dataframe1, dataframe2, output = "report"), structure(list(samplespecies_id = c("1", "2"), logical = c(
    TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
})

