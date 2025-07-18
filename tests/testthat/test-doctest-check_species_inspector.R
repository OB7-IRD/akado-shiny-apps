# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_species_inspector", {
  # Created from @doctest for `check_species_inspector`
  # Source file: R/function.R
  # Source line: 3673
  dataframe1 <- data.frame(samplespecies_id = c("1", "2"), species_fao_code = c("YFT", "JOS"))
  expect_equal(check_species_inspector(dataframe1, output = "report"), structure(list(samplespecies_id = c("1", "2"), logical = c(TRUE, FALSE),
  species_fao_code = c("YFT", "JOS")), row.names = c(NA, -2L), class = "data.frame"))
})

