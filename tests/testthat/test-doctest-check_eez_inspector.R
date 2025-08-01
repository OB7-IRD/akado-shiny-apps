# Generated by doctest: do not edit by hand
# Please edit file in R/function.R

test_that("Doctest: check_eez_inspector", {
  # Created from @doctest for `check_eez_inspector`
  # Source file: R/function.R
  # Source line: 3443
  dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"),
  fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"),
  activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)", "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)", "POINT (1 1)", "POINT (6 6)"))
  dataframe2 <- sf::st_sf(data.frame(ISO_TER1 = c("SYC", "XSG"), ISO_TER2 = c(NA, NA), ISO_TER3 = c(NA, NA), geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(
    0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0)))), sf::st_polygon(list(rbind(c(3, 3), c(3, 5), c(5, 5), c(5, 3), c(3, 3)))), crs = 4326)))
  expect_equal(check_eez_inspector(dataframe1, dataframe2, output = "report"), list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
  vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c(
    "SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), eez_calculated = c("SYC", "XSG", NA, "On land or in international waters", NA, NA, "SYC",
    "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA, 8L), class = "data.frame"),
  structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c(
    "SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), activity_position = c(
    "POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)", "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)", "POINT (1 1)", "POINT (6 6)"), eez_calculated = c("SYC",
    "XSG", NA, "On land or in international waters", NA, NA, "SYC", "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE,
    FALSE, FALSE), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, 8L), class = "data.frame")))
})

