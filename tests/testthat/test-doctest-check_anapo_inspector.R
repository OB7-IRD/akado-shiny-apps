# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_anapo_inspector", {
  # Created from @doctest for `check_anapo_inspector`
  # Source file: R/function.R
  # Source line: 6251
  dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6"), activity_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12", "2020/01/13",
    "2020/01/12", "2020/01/12")), activity_time = c("05:26:01", "10:41:15", "16:41:15", "03:12:34", "05:56:12", "23:26:47"), activity_position = c(
    "POINT (1 1)", "POINT (0 0)", "POINT (3 0)", "POINT (4 4)", NA, "POINT (3 0.6)"), trip_id = c("1", "2", "2", "2", "2", "2"))
  dataframe2 <- data.frame(trip_id = c("1", "2"), vessel_code = c("1", "1"), harbour_position_departure = c("POINT (1 1.1)", "POINT (3 3)"),
  harbour_position_landing = c("POINT (3 3)", "POINT (3 3)"))
  dataframe3 <- data.frame(vms_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12")), vms_time = c("15:26:01", "10:55:15", "22:32:17"), vms_position = c(
    "POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"), vessel_code = c("1", "1", "1"))
  expect_equal(check_anapo_inspector(dataframe1, dataframe2, dataframe3, output = "report", threshold_number_vms = 1), list(structure(list(activity_id = c("1",
    "2", "3", "4", "5", "6"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), nb_vms = c(1L, 2L, 2L, NA, 2L, 2L), min_distance = structure(c(NA,
    6.004055139173, 18.012165417519, 230.106883933216, NA, 18.012165417519), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"),
  class = "units"), max_score = c(NA, NA, 2.17959673670807, 0, NA, 0.209441144555651)), row.names = c(NA, 6L), class = "data.frame"), structure(list(
    activity_id = c("3", "3", "4", "4", "6", "6", "1", "2", "2", "5", "5"), activity_date = structure(c(18273, 18273, 18274, 18274, 18273, 18273, 18262, 18273,
      18273, 18273, 18273), class = "Date"), activity_time = c("16:41:15", "16:41:15", "03:12:34", "03:12:34", "23:26:47", "23:26:47", "05:26:01", "10:41:15",
      "10:41:15", "05:56:12", "05:56:12"), activity_position = c("POINT (3 0)", "POINT (3 0)", "POINT (4 4)", "POINT (4 4)", "POINT (3 0.6)", "POINT (3 0.6)",
      "POINT (1 1)", "POINT (0 0)", "POINT (0 0)", NA, NA), vms_date = structure(c(18273, 18273, 18273, 18273, 18273, 18273, 18262, 18273, 18273, 18273, 18273),
    class = "Date"), vms_time = c("10:55:15", "22:32:17", "10:55:15", "22:32:17", "10:55:15", "22:32:17", "15:26:01", "10:55:15", "22:32:17", "10:55:15",
      "22:32:17"), vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)",
      "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)"), distance = structure(c(180.221602566745, 18.012165417519, 335.278629168604,
      230.106883933216, 182.602328607533, 18.012165417519, NA, 6.004055139173, 181.019203021658, NA, NA), units = structure(list(numerator = "NM",
      denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(20760000, -21062000, -27761000, -69583000, 45092000,
      3270000, NA, NA, NA, NA, NA), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(
      0, 2.17959673670807, 0, 0, 0, 0.209441144555651, NA, NA, NA, NA, NA), vms_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326),
    activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, -11L), class = "data.frame")))
})

