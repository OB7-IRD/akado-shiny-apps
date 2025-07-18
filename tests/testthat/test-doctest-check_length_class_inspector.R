# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_length_class_inspector", {
  # Created from @doctest for `check_length_class_inspector`
  # Source file: R/function.R
  # Source line: 2373
  dataframe1 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"), species_fao_code = c("YFT", "YFT", "LTA", "YFT", "YFT"),
  sizemeasuretype_code = c("FL", "PD1", "FL", "FL", "FL"), samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA))
  expect_equal(check_length_class_inspector(dataframe1, output = "report"), structure(list(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"), logical = c(
    TRUE, TRUE, TRUE, FALSE, FALSE), samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA)), row.names = c(NA, 5L), class = "data.frame"))
})

