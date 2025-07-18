# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_weight_inspector", {
  # Created from @doctest for `check_weight_inspector`
  # Source file: R/function.R
  # Source line: 2200
  dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4"), activity_weight = c(10, 15.01, 6, NA))
  dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5"), catch_weight = c(10, 10, 5, 3, 2), activity_id = c("1", "2", "2", "3", "3"))
  expect_equal(check_weight_inspector(dataframe1, dataframe2, output = "report"), structure(list(activity_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE,
    FALSE, TRUE), activity_weight = c(10, 15.01, 6, NA), sum_catch_weight = c(10, 15, 5, NA)), row.names = c(NA, 4L), class = "data.frame"))
})

