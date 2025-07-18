# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_well_number_consistent_inspector", {
  # Created from @doctest for `check_well_number_consistent_inspector`
  # Source file: R/function.R
  # Source line: 4247
  dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5"), sample_well = c("well_1", "well_2", "well_2", "well_2", NA), trip_id = c("1", "1", "2", "3",
    "3"))
  dataframe2 <- data.frame(well_id = c("1", "2", "3"), trip_id = c("1", "1", "3"), well_label = c("well_1", "well_2", "well_1"))
  dataframe3 <- data.frame(trip_id = c("1", "2", "3"), vesseltype_code = c("6", "1", "6"))
  expect_equal(check_well_number_consistent_inspector(dataframe1, dataframe2, dataframe3, output = "report"), structure(list(sample_id = c("1", "2", "3", "4",
    "5"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), sample_well = c("well_1", "well_2", "well_2", "well_2", NA)), row.names = c(NA, 5L), class = "data.frame"))
})

