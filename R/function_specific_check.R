
#' @name display_raising_factor
#' @title Function to change table display raising_factor
#' @description Changes the number of decimal places in RF1
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the first output of the function {\link[codama]{check_raising_factor_inspector}}, which must be done before using the display_raising_factor () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  rf1}}
#' }
#' @return The function returns a {\link[base]{data.frame}} with the modified RF1
#' @doctest
#' dataframe1 <- data.frame(rf1 = c(1, 1.11111111111111, 2.55555))
#' @expect equal(., structure(list(rf1 = c(1, 1.11111, 2.55555)), row.names = c(NA, -3L), class = "data.frame"))
#' display_raising_factor(dataframe1)
#' @export
display_raising_factor <- function(dataframe1) {
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("rf1"),
    column_type = c("numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("rf1"),
      column_type = c("numeric"),
      output = "error"
    )
  }
  # 2 - Data design ----
  dataframe1$rf1 <- trunc(dataframe1$rf1 * 100000) / 100000
  return(dataframe1)
}

#' @name display_little_big
#' @title Function to change table display little_big
#' @description Changes the number of decimal places in percentage
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the first output of the function {\link[codama]{check_little_big_inspector}}, which must be done before using the display_little_big () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  little_percentage}}
#'  \item{\code{  big_percentage}}
#'  \item{\code{  measure1_percentage}}
#'  \item{\code{  measure2_percentage}}
#' }
#' @return The function returns a {\link[base]{data.frame}} with the modified percentage
#' @doctest
#' dataframe1 <- data.frame(little_percentage = c(0.0001, 0.19),
#'                          big_percentage = c(0.1999, 0.61),
#'                          measure1_percentage = c(0.5, 0.005),
#'                          measure2_percentage = c(0.3, 0.195))
#' @expect equal(., structure(list(little_percentage = c(0, 19), big_percentage = c(19.9, 61), measure1_percentage = c(50, 0.5), measure2_percentage = c(30, 19.5)), row.names = c(NA, -2L), class = "data.frame"))
#' display_little_big(dataframe1)
#' @export
display_little_big <- function(dataframe1) {
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("little_percentage", "big_percentage", "measure1_percentage", "measure2_percentage"),
    column_type = c("numeric", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("little_percentage", "big_percentage", "measure1_percentage", "measure2_percentage"),
      column_type = c("numeric", "numeric", "numeric", "numeric"),
      output = "error"
    )
  }
  # 2 - Data design ----
  dataframe1$little_percentage <- trunc(dataframe1$little_percentage * 1000) / 10
  dataframe1$big_percentage <- trunc(dataframe1$big_percentage * 1000) / 10
  dataframe1$measure1_percentage <- trunc(dataframe1$measure1_percentage * 1000) / 10
  dataframe1$measure2_percentage <- trunc(dataframe1$measure2_percentage * 1000) / 10
  return(dataframe1)
}

#' @name plot_temporal_limit_data
#' @title Function to create list data/argument for the plot plot_temporal_limit
#' @description Function to create list data/argument
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_temporal_limit_inspector}}, which must be done before using the plot_temporal_limit_data () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_temporal_limit_data () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_startdate}}
#'  \item{\code{  trip_enddate}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  inter_activity_date}}
#'  \item{\code{  exter_activity_date}}
#'  \item{\code{  count_freq}}
#'  \item{\code{  logical}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vessel_code}}
#' }
#' @return The function returns a {\link[base]{list}} with the sub list is a data for one plot
#' @doctest
#' dataframe1 <- data.frame(trip_id = c("1", "1", "2", "2", "2"),
#'                          trip_startdate = as.Date(c("2020/01/01", "2020/01/01", "2020/01/01",
#'                                                     "2020/01/01", "2020/01/01")),
#'                          trip_enddate = as.Date(c("2020/01/02", "2020/01/02", "2020/01/02",
#'                                                   "2020/01/02", "2020/01/02")),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/02","2020/01/01",
#'                                                    "2020/01/02", "2020/01/03")),
#'                          inter_activity_date = c(TRUE, TRUE, TRUE, TRUE, FALSE),
#'                          exter_activity_date = c(FALSE, FALSE, FALSE, FALSE, TRUE),
#'                          count_freq = c(1L, 1L, 1L, 1L, 1L),
#'                          logical = c(TRUE, TRUE, TRUE, TRUE, FALSE))
#' dataframe2 <- data.frame(trip_id = c("1", "2"),
#'                          vessel_code = c("0", "0"))
#' @expect equal(., list(`1` = list(data = structure(list(activity_date = structure(c(18262, 18263), class = "Date"), logical = c(TRUE, TRUE), count_freq = c(1L, 1L)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame")), startdate = structure(18262, class = "Date"), enddate = structure(18263, class = "Date"), vessel_code = "0"), `2` = list(data = structure(list(activity_date = structure(c(18262, 18263, 18264), class = "Date"), logical = c(TRUE, TRUE, FALSE), count_freq = c(1L, 1L, 1L)), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), startdate = structure(18262, class = "Date"), enddate = structure(18263, class = "Date"), vessel_code = "0")))
#' plot_temporal_limit_data(dataframe1, dataframe2)
#' @export
plot_temporal_limit_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  trip_startdate <- NULL
  trip_enddate <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_startdate", "trip_enddate", "activity_date", "inter_activity_date", "exter_activity_date", "count_freq", "logical"),
    column_type = c("character", "Date", "Date", "Date", "logical", "logical", "integer", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_startdate", "trip_enddate", "activity_date", "inter_activity_date", "exter_activity_date", "count_freq", "logical"),
      column_type = c("character", "Date", "Date", "Date", "logical", "logical", "integer", "logical"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_startdate", "trip_enddate", "activity_date", "inter_activity_date", "exter_activity_date", "count_freq", "logical")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "vessel_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "vessel_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "vessel_code")]
  }
  # 2 - Data design ----
  check_temporal_limit_data_plot <- dataframe1
  # Add missing date
  check_temporal_limit_data_plot <- as.data.frame(check_temporal_limit_data_plot) %>%
    dplyr::group_by(trip_id) %>%
    tidyr::complete(activity_date = seq.Date(min(trip_startdate[1], trip_enddate[1]), max(trip_startdate[1], trip_enddate[1]), by = "day"), trip_startdate = trip_startdate[1], trip_enddate = trip_enddate[1])
  # Replaces NA for missing dates
  check_temporal_limit_data_plot <- check_temporal_limit_data_plot %>% tidyr::replace_na(list(inter_activity_date = TRUE, exter_activity_date = FALSE, count_freq = 0, logical = FALSE))
  # Retrieving information for the plot
  check_temporal_limit_data_plot <- dplyr::inner_join(check_temporal_limit_data_plot, dataframe2[, c("trip_id", "vessel_code")], by = dplyr::join_by(trip_id))
  check_temporal_limit_data_plot <- data_to_list(data = check_temporal_limit_data_plot, name_col_dataplot = "data", colname_id = "trip_id", colname_plot = c("activity_date", "logical", "count_freq"), colname_info = c("trip_startdate", "trip_enddate", "vessel_code"), rename_colname_info = c("startdate", "enddate", "vessel_code"))
  return(check_temporal_limit_data_plot)
}


#' @name plot_temporal_limit
#' @title Function to create the plot of the consistency of the dates by trip
#' @description Function to create the plot
#' @param data {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_temporal_limit_data}}, which must be done before using the plot_temporal_limit () function.
#' @param startdate {\link[base]{Date}} expected. Trip start date
#' @param enddate {\link[base]{Date}} expected. Trip end date
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data :
#'  \item{\code{  activity_date}}
#'  \item{\code{  count_freq}}
#'  \item{\code{  logical}}
#' }
#' @return The function returns a {\link[plotly]{plotly}}
#' # Example 1
#' data_1 <- data.frame(activity_date = as.Date(c("2020/01/01", "2020/01/02")),
#'                    logical = c(TRUE, TRUE),
#'                    count_freq = c(1L, 1L))
#' plot_temporal_limit(data_1, as.Date("2020/01/01"), as.Date("2020/01/02"))
#' # Example 2
#' data_2 <- data.frame(activity_date = as.Date(c("2020/01/01", "2020/01/02", "2020/01/03")),
#'                    logical = c(TRUE, TRUE, FALSE),
#'                    count_freq = c(1L, 1L, 1L))
#' plot_temporal_limit(data_2, as.Date("2020/01/01"), as.Date("2020/01/02"))
#' @export
plot_temporal_limit <- function(data, startdate, enddate) {
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("activity_date", "count_freq", "logical"),
    column_type = c("Date", "integer", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("activity_date", "count_freq", "logical"),
      column_type = c("Date", "integer", "logical"),
      output = "error"
    )
  } else {
    data <- data[, c("activity_date", "count_freq", "logical")]
  }
  if (!codama::r_type_checking(
    r_object = startdate,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = startdate,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = enddate,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = enddate,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Deletes the rows where the date of the activity is missing
  data <- data[!is.na(data$activity_date), ]
  # 3 - Plot ----
  plotly::plot_ly() %>%
    plotly::add_markers(x = c(startdate, enddate), y = c(1, 1), marker = list(
      color = "#63A9FF", symbol = "circle"
    ), name = "start date and end date", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    plotly::add_markers(data = subset(data, logical == TRUE), x = ~activity_date, y = ~count_freq, marker = list(
      color = "#18ED84", symbol = "cross-thin-open"
    ), name = "date activity good", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    plotly::add_markers(data = subset(data, logical == FALSE), x = ~activity_date, y = ~count_freq, marker = list(
      color = "#FF7320", symbol = "x-thin-open"
    ), name = "date activity bad", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    plotly::layout(
      xaxis = list(
        title = "Date",
        dtick = 86400000.0 * 5,
        tickformat = "%b %d"
      ),
      yaxis = list(
        title = "Occurence",
        tickvals = c(data$count_freq, 1),
        ticktext = c(data$count_freq, 1)
      ),
      legend = list(orientation = "h", y = -0.3)
    )
}


#' @name plot_temporal_limit_windows
#' @title Function to create the text for windows with plot temporal
#' @description Function to create the text for windows with plot of the consistency of the dates by trip
#' @param vessel_code {\link[base]{character}} expected. Vessel code
#' @param enddate {\link[base]{Date}} expected. Trip end date
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., "<b>Trip information : </b><br>\n          <ul><li>Vessel code : 0</li>\n          <li>Trip end date : 2020-01-02</li></ul>")
#' plot_temporal_limit_windows("0", as.Date("2020/01/02"))
#' @export
plot_temporal_limit_windows <- function(vessel_code, enddate) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = vessel_code,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_code,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = enddate,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = enddate,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  paste0("<b>Trip information : </b><br>
          <ul><li>Vessel code : ", vessel_code, "</li>
          <li>Trip end date : ", enddate, "</li></ul>")
}

#' @name plot_position_data
#' @title Function to create list data/argument for the plot plot_position
#' @description Function to create list data/argument
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_position_inspector}}, which must be done before using the plot_position_data () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_position_data () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#'  \item{\code{  type}}
#'  \item{\code{  ocean_label}}
#'  \item{\code{  ocean_calculate}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  trip_enddate}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_number}}
#' }
#' @return The function returns a {\link[base]{list}} with the sub list is a data for one plot
#' @doctest
#' dataframe1 <- data.frame(activity_id = c("1", "2"),
#'                          activity_position = c("POINT (0 0)", "POINT (0 0)"),
#'                          type = c("Sea", "Sea"),
#'                          ocean_label = c("ocean_1", "ocean_2"),
#'                          ocean_calculate = c("ocean_1", "ocean_1"),
#'                          activity_crs = c(4326, 4326))
#' dataframe2 <- data.frame(activity_id = c("1", "2"),
#'                          vessel_code = c("0", "0"),
#'                          trip_enddate = as.Date(c("2020/01/10", "2020/01/10")),
#'                          activity_date = as.Date(c("2020/01/09", "2020/01/10")),
#'                          activity_number = c(1L, 1L))
#' @expect equal(., list(`1` = list(data = structure(list(activity_position = "POINT (0 0)", activity_crs = 4326), class = "data.frame", row.names = 1L), vessel_code = "0", trip_enddate = structure(18271, class = "Date"), activity_date = structure(18270, class = "Date"), activity_number = 1L, type = "Sea", ocean_label = "ocean_1", ocean_calculate = "ocean_1", X = "E0°0'", Y = "N0°0'"), `2` = list(data = structure(list(activity_position = "POINT (0 0)", activity_crs = 4326), class = "data.frame", row.names = 2L), vessel_code = "0", trip_enddate = structure(18271, class = "Date"), activity_date = structure(18271, class = "Date"), activity_number = 1L, type = "Sea", ocean_label = "ocean_2", ocean_calculate = "ocean_1", X = "E0°0'", Y = "N0°0'")))
#' plot_position_data(dataframe1, dataframe2)
#' @export
plot_position_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  . <- NULL
  activity_position <- NULL
  X <- NULL
  Y <- NULL
  activity_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_position", "activity_crs", "type", "ocean_label", "ocean_calculate"),
    column_type = c("character", "character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_position", "activity_crs", "type", "ocean_label", "ocean_calculate"),
      column_type = c("character", "character", "numeric", "character", "character", "character"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number"),
    column_type = c("character", "character", "Date", "Date", "integer"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number"),
      column_type = c("character", "character", "Date", "Date", "integer"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number")]
  }
  # 2 - Data design ----
  # Retrieves X, Y coordinates of position
  dataframe1_geo <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(wkt = "activity_position", crs = "4326", remove = FALSE) %>%
    dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
    dplyr::mutate(X = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe1_geo[, c("activity_id", "X", "Y")], by = dplyr::join_by(activity_id))
  # Retrieving information for the plot
  dataframe1 <- dplyr::inner_join(dataframe1, dataframe2[, c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number")], by = dplyr::join_by(activity_id))
  dataframe1 <- data_to_list(data = dataframe1, name_col_dataplot = "data", colname_id = "activity_id", colname_plot = c("activity_position", "activity_crs"), colname_info = c("vessel_code", "trip_enddate", "activity_date", "activity_number", "type", "ocean_label", "ocean_calculate", "X", "Y"), rename_colname_info = c("vessel_code", "trip_enddate", "activity_date", "activity_number", "type", "ocean_label", "ocean_calculate", "X", "Y"))
  return(dataframe1)
}

#' @name plot_position
#' @title Function to create the plot of the consistency of the position for the activity
#' @description Function to create the plot
#' @param data {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_position_data}}, which must be done before using the plot_position () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data :
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#' }
#' @return The function returns a {\link[plotly]{plotly}}
#' # Example 1
#' data <- data.frame(activity_position = "POINT (0 0)",
#'                    activity_crs = 4326)
#' plot_position(data)
#' @export
plot_position <- function(data) {
  # 0 - Global variables assignement ----
  . <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("activity_position", "activity_crs"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("activity_position", "activity_crs"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    data <- data[, c("activity_position", "activity_crs")]
  }
  # 2 - Data design ----
  # Spatial formatting
  if (!is.na(data$activity_position)) {
    data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = unique(data$activity_crs)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE)) %>%
      dplyr::mutate(text = paste("Position:", latitude, ",", longitude, "<extra></extra>"))
    # 3 - Plot ----
    plotly::plot_ly(data = data_geo, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", marker = list(size = 10), hovertemplate = ~text) %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron", center = list(lon = data_geo$X, lat = data_geo$Y)))
  } else {
    # Plot
    plotly::plot_ly(type = "scattermapbox", mode = "markers", marker = list(size = 10)) %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron"))
  }
}

#' @name plot_position_windows
#' @title Function to create the text for windows with plot position
#' @description Function to create the windows with plot of the consistency of the position for the activity
#' @param vessel_code {\link[base]{character}} expected. Vessel code
#' @param trip_enddate {\link[base]{Date}} expected. Trip end date
#' @param activity_date {\link[base]{Date}} expected. Activity date
#' @param activity_number {\link[base]{integer}} expected. Activity number
#' @param X {\link[base]{character}} expected. Position longitude
#' @param Y {\link[base]{character}} expected. Position latitude
#' @param type {\link[base]{character}} expected. Type of inconsistency
#' @param ocean_label {\link[base]{character}} expected. Ocean label
#' @param ocean_calculate {\link[base]{character}} expected. Ocean calculate
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., "<b>Trip information : </b><br>\n          <ul><li>Vessel code : 0</li>\n          <li>Trip end date : 2020-01-10</li>\n          <li>Activity date : 2020-01-09</li>\n          <li>Activity number : 1</li>\n          <li>Latitude : N0°0'</li>\n          <li>Longitude : E0°0'</li></ul>\n          <b>Problem information : </b><br>\n          <ul><li>Type : Sea</li>\n          <li>Ocean trip : ocean_1</li>\n          <li>Ocean activity : ocean_1</li></ul>")
#' plot_position_windows(vessel_code = "0", trip_enddate = as.Date("2020/01/10"),
#'                       activity_date = as.Date("2020/01/09"), activity_number = 1L, X = "E0°0'",
#'                       Y = "N0°0'", type = "Sea", ocean_label = "ocean_1",
#'                       ocean_calculate = "ocean_1")
#' @export
plot_position_windows <- function(vessel_code, trip_enddate, activity_date, activity_number, X, Y, type, ocean_label, ocean_calculate) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = vessel_code,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_code,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = trip_enddate,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = trip_enddate,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_date,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_date,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_number,
    type = "integer",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_number,
      type = "integer",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = Y,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = Y,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = X,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = X,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = type,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = type,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = ocean_label,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = ocean_label,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = ocean_calculate,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = ocean_calculate,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  paste0("<b>Trip information : </b><br>
          <ul><li>Vessel code : ", vessel_code, "</li>
          <li>Trip end date : ", trip_enddate, "</li>
          <li>Activity date : ", activity_date, "</li>
          <li>Activity number : ", activity_number, "</li>
          <li>Latitude : ", Y, "</li>
          <li>Longitude : ", X, "</li></ul>
          <b>Problem information : </b><br>
          <ul><li>Type : ", type, "</li>
          <li>Ocean trip : ", ocean_label, "</li>
          <li>Ocean activity : ", ocean_calculate, "</li></ul>")
}

#' @name plot_eez_data
#' @title Function to create list data/argument for the plot eez
#' @description Function to create list data/argument
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_eez_inspector}}, which must be done before using the plot_eez_data () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_eez_data () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#'  \item{\code{  fpazone_code}}
#'  \item{\code{  fpazone_country_iso3}}
#'  \item{\code{  eez_calculated}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  trip_enddate}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_number}}
#' }
#' @return The function returns a {\link[base]{list}} with the sub list is a data for one plot
#' @doctest
#' dataframe1 <- data.frame(activity_id = c("1", "2"),
#'                          activity_position = c("POINT (0.1 0)", "POINT (0 0)"),
#'                          fpazone_code = c("SYC", "XSG"),
#'                          fpazone_country_iso3 = c("SYC", "XXX"),
#'                          eez_calculated = c("SYC", "XSG"),
#'                          activity_crs = c(4326, 4326))
#' dataframe2 <- data.frame(activity_id = c("1", "2"),
#'                          vessel_code = c("0", "0"),
#'                          trip_enddate = as.Date(c("2020/01/10", "2020/01/10")),
#'                          activity_date = as.Date(c("2020/01/09", "2020/01/10")),
#'                          activity_number = c(1L, 1L))
#' @expect equal(., list(`1` = list(data = structure(list(activity_position = "POINT (0.1 0)", activity_crs = 4326), class = "data.frame", row.names = 1L), vessel_code = "0", trip_enddate = structure(18271, class = "Date"), activity_date = structure(18270, class = "Date"), activity_number = 1L, fpazone_code = "SYC", fpazone_country_iso3 = "SYC", eez_calculated = "SYC", X = "E0°6'", Y = "N0°0'", referential_geographical_shape = "shape_eez"), `2` = list(data = structure(list(activity_position = "POINT (0 0)", activity_crs = 4326), class = "data.frame", row.names = 2L), vessel_code = "0", trip_enddate = structure(18271, class = "Date"), activity_date = structure(18271, class = "Date"), activity_number = 1L, fpazone_code = "XSG", fpazone_country_iso3 = "XXX", eez_calculated = "XSG", X = "E0°0'", Y = "N0°0'", referential_geographical_shape = "shape_eez")))
#' plot_eez_data(dataframe1, dataframe2)
#' @export
plot_eez_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  . <- NULL
  activity_position <- NULL
  X <- NULL
  Y <- NULL
  activity_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_position", "activity_crs", "fpazone_code", "fpazone_country_iso3", "eez_calculated"),
    column_type = c("character", "character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_position", "activity_crs", "fpazone_code", "fpazone_country_iso3", "eez_calculated"),
      column_type = c("character", "character", "numeric", "character", "character", "character"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number"),
    column_type = c("character", "character", "Date", "Date", "integer"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number"),
      column_type = c("character", "character", "Date", "Date", "integer"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number")]
  }
  # 2 - Data design ----
  check_eez_data_plot <- dataframe1
  # Retrieves X, Y coordinates of position
  check_eez_data_plot_geo <- check_eez_data_plot %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(wkt = "activity_position", crs = "4326", remove = FALSE) %>%
    dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
    dplyr::mutate(X = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  check_eez_data_plot <- dplyr::left_join(check_eez_data_plot, check_eez_data_plot_geo[, c("activity_id", "X", "Y")], by = dplyr::join_by(activity_id))
  # Retrieving information for the plot
  check_eez_data_plot <- dplyr::inner_join(check_eez_data_plot, dataframe2[, c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number")], by = dplyr::join_by(activity_id))
  # Add names of file referential geographical shape (store in referential_file)
  check_eez_data_plot$referential_geographical_shape <- "shape_eez"
  check_eez_data_plot <- data_to_list(data = check_eez_data_plot, name_col_dataplot = "data", colname_id = "activity_id", colname_plot = c("activity_position", "activity_crs"), colname_info = c("vessel_code", "trip_enddate", "activity_date", "activity_number", "fpazone_code", "fpazone_country_iso3", "eez_calculated", "X", "Y", "referential_geographical_shape"), rename_colname_info = c("vessel_code", "trip_enddate", "activity_date", "activity_number", "fpazone_code", "fpazone_country_iso3", "eez_calculated", "X", "Y", "referential_geographical_shape"))
  return(check_eez_data_plot)
}

#' @name plot_eez
#' @title Function to create the plot of the consistency of the eez for the activity
#' @description Function to create the plot
#' @param data {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_eez_data}}, which must be done before using the plot_eez () function.
#' @param referential_geographical_shape {\link[base]{data.frame}} expected. Layer to containing the eez shapefile (example cf : Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632)
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data :
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#' }
#' \itemize{
#' referential_geographical_shape :
#'  \item{\code{  ISO_TER1}}
#'  \item{\code{  ISO_TER2}}
#'  \item{\code{  ISO_TER3}}
#'  \item{\code{  geometry}}
#' }
#' @return The function returns a {\link[plotly]{plotly}}
#' # Example 1
#' data <- data.frame(activity_position = "POINT (0.1 0)",
#'                    activity_crs = 4326)
#' referential_geographical_shape <- sf::st_sf(data.frame(ISO_TER1 = c("SYC", "XSG"),
#'                                    ISO_TER2 = c(NA, NA),
#'                                    ISO_TER3 = c(NA, NA),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          sf::st_polygon(list(rbind(c(3,3), c(3,5),
#'                                                                                    c(5,5), c(5,3),
#'                                                                                    c(3,3)))),
#'                                                          crs = 4326)))
#' plot_eez(data, referential_geographical_shape)
#' @export
plot_eez <- function(data, referential_geographical_shape) {
  # 0 - Global variables assignement ----
  . <- NULL
  ISO_TER1 <- NULL
  ISO_TER2 <- NULL
  ISO_TER3 <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("activity_position", "activity_crs"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("activity_position", "activity_crs"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    data <- data[, c("activity_position", "activity_crs")]
  }
  if (!codama::r_table_checking(
    r_table = referential_geographical_shape,
    type = "data.frame",
    column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = referential_geographical_shape,
      type = "data.frame",
      column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
      output = "error"
    )
  } else {
    referential_geographical_shape <- referential_geographical_shape[, c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry")]
  }
  # 2 - Data design ----
  # Remove empty geometry
  referential_geographical_shape <- referential_geographical_shape[!sf::st_is_empty(referential_geographical_shape), ]
  # text hovertemplate
  referential_geographical_shape <- referential_geographical_shape %>% dplyr::mutate(text = paste("Country 1:", ISO_TER1, "<br>Country 2:", ISO_TER2, "<br>Country 3:", ISO_TER3, "<extra></extra>"))
  # 3 - Plot ----
  # Spatial formatting
  plot <- plotly::plot_ly()
  if (!is.na(data$activity_position)) {
    data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = unique(data$activity_crs)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE)) %>%
      dplyr::mutate(text = paste("Position:", latitude, ",", longitude, "<extra></extra>"))
    # Plot
    plot <- plot %>%
      plotly::add_trace(name = "Activity", data = data_geo, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", marker = list(size = 10), hovertemplate = ~text) %>%
      plotly::layout(mapbox = list(style = "carto-positron", center = list(lon = data_geo$X, lat = data_geo$Y)))
  }else {
    # Plot
    plot <- plot %>%
      plotly::layout(mapbox = list(style = "carto-positron"))
  }
  plot <- plot %>%
    plotly::add_sf(name = "EEZ", data = referential_geographical_shape, type = "scattermapbox",  mode = "lines", hovertemplate = ~text, fill = "none")
  return(plot)
}

#' @name plot_eez_windows
#' @title Function to create the text for windows with plot eez
#' @description Function to create the windows with plot of the consistency of the eez for the activity
#' @param vessel_code {\link[base]{character}} expected. Vessel code
#' @param trip_enddate {\link[base]{Date}} expected. Trip end date
#' @param activity_date {\link[base]{Date}} expected. Activity date
#' @param activity_number {\link[base]{integer}} expected. Activity number
#' @param X {\link[base]{character}} expected. Position longitude
#' @param Y {\link[base]{character}} expected. Position latitude
#' @param fpazone_code {\link[base]{character}} expected. Fpa zone code
#' @param fpazone_country_iso3 {\link[base]{character}} expected. Iso3 country code for fpa zone
#' @param eez_calculated {\link[base]{character}} expected. Calculated zee zone code
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., "<b>Trip information : </b><br>\n         <ul><li>Vessel code : 0</li>\n         <li>Trip end date : 2020-01-10</li>\n         <li>Activity date : 2020-01-09</li>\n         <li>Activity number : 1</li>\n         <li>Latitude : N0°0'</li>\n         <li>Longitude : E0°6'</li></ul>\n         <b>Problem information : </b><br>\n         <ul><li>Declared eez : SYC</li>\n         <li>Declared country eez : SYC</li>\n         <li>Calculated eez : SYC</li></ul>")
#' plot_eez_windows(vessel_code = "0", trip_enddate = as.Date("2020/01/10"),
#'                  activity_date = as.Date("2020/01/09"), activity_number = 1L, X = "E0°6'",
#'                  Y = "N0°0'", fpazone_code = "SYC", fpazone_country_iso3 = "SYC",
#'                  eez_calculated = "SYC")
#' @export
plot_eez_windows <- function(vessel_code, trip_enddate, activity_date, activity_number, X, Y, fpazone_code, fpazone_country_iso3, eez_calculated) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = vessel_code,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_code,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = trip_enddate,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = trip_enddate,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_date,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_date,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_number,
    type = "integer",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_number,
      type = "integer",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = Y,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = Y,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = X,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = X,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = fpazone_code,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = fpazone_code,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = fpazone_country_iso3,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = fpazone_country_iso3,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = eez_calculated,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = eez_calculated,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  paste0("<b>Trip information : </b><br>
         <ul><li>Vessel code : ", vessel_code, "</li>
         <li>Trip end date : ", trip_enddate, "</li>
         <li>Activity date : ", activity_date, "</li>
         <li>Activity number : ", activity_number, "</li>
         <li>Latitude : ", Y, "</li>
         <li>Longitude : ", X, "</li></ul>
         <b>Problem information : </b><br>
         <ul><li>Declared eez : ", fpazone_code, "</li>
         <li>Declared country eez : ", fpazone_country_iso3, "</li>
         <li>Calculated eez : ", eez_calculated, "</li></ul>")
}

#' @name display_anapo
#' @title Function to change table display anapo
#' @description Adds information about groundings and previous and next activities
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_anapo_inspector}}, which must be done before using the display_anapo () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the display_anapo () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the display_anapo () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or the first output of the function {\link[codama]{check_anapo_inspector}}, which must be done before using the display_anapo () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_position}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_number}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoyoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  activity_id}}
#'  \item{\code{  logical}}
#'  \item{\code{  nb_vms}}
#'  \item{\code{  min_distance}}
#'  \item{\code{  max_score}}
#' }
#' @return The function returns a {\link[base]{data.frame}}
#' @doctest
#' dataframe1 <- data.frame(activity_id  = c("2", "4", "6"),
#'                          activity_date = as.Date(c("2020/01/12", "2020/01/13", "2020/01/13")),
#'                          activity_time = c("05:26:01", "03:12:34", "23:26:47"),
#'                          activity_position = c("POINT (0 0)", "POINT (4 4)", "POINT (3 0.6)"))
#' dataframe2 <- data.frame(trip_id  = c("1", "1", "1"),
#'                          activity_id = c("2", "4", "6"),
#'                          activity_number = c(1L, 1L, 2L))
#' dataframe3 <- data.frame(transmittingbuoy_id  = c("1"),
#'                          transmittingbuoyoperation_code = c("4"),
#'                          activity_id = c("6"))
#' dataframe4 <- data.frame(activity_id  = c("2", "4", "6"),
#'                          logical = c(TRUE, FALSE, FALSE),
#'                          nb_vms = c(1L, NA, 2L),
#'                          min_distance = c(6.004055, 84.717419, 18.012165),
#'                          max_score = c(NA, 0.0000000, 0.2094411))
#' units::install_unit("NM", "1852 m", "Nautical mile")
#' dataframe4$min_distance <- units::set_units(dataframe4$min_distance, NM)
#' @expect equal(., structure(list(activity_id = c("2", "4", "6"), logical = c(TRUE, FALSE, FALSE), nb_vms = c(1L, NA, 2L), min_distance = structure(c(6.004, 84.717, 18.012), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), max_score = c(NA, 0, 0.209), grounding = c(FALSE, FALSE, TRUE), activity_position = c("POINT (0 0)", "POINT (4 4)", "POINT (3 0.6)"), activity_position_prior = c(NA, "POINT (0 0)", "POINT (4 4)"), activity_position_post = c("POINT (4 4)", NA, NA)), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")))
#' display_anapo(dataframe1, dataframe2, dataframe3, dataframe4)
#' @export
display_anapo <- function(dataframe1, dataframe2, dataframe3, dataframe4) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  trip_id <- NULL
  activity_position <- NULL
  grounding <- NULL
  activity_date <- NULL
  activity_number <- NULL
  activity_position_prior <- NULL
  activity_position_post <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "activity_position"),
    column_type = c("character", "Date", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "activity_position"),
      column_type = c("character", "Date", "character", "character"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "trip_id", "activity_number"),
    column_type = c("character", "character", "integer"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "trip_id", "activity_number"),
      column_type = c("character", "character", "integer"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "trip_id", "activity_number")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("activity_id", "logical", "nb_vms", "min_distance", "max_score"),
    column_type = c("character", "logical", "integer", "units", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("activity_id", "logical", "nb_vms", "min_distance", "max_score"),
      column_type = c("character", "logical", "integer", "units", "numeric"),
      output = "error"
    )
  }
  # 2 - Data design ----
  check_anapo_inspector_dataplot <- dplyr::inner_join(dataframe1, dataframe2[, c("activity_id", "trip_id", "activity_number")], by = dplyr::join_by(activity_id))
  # Add information on whether the activity is linked to a grounding (object or buoy) or not in data plot
  data_tmp_grounding <- column_grounding(data = check_anapo_inspector_dataplot, data_transmittingbuoy = dataframe3)
  check_anapo_inspector_dataplot <- dplyr::inner_join(check_anapo_inspector_dataplot, data_tmp_grounding, by = dplyr::join_by(activity_id))
  # Selecting useful data for the plot
  check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot %>%
    dplyr::select("trip_id", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "grounding") %>%
    dplyr::group_by(trip_id) %>%
    dplyr::distinct()
  # Add position information for activities n, n-1 and n+1 (not just related to grounding)
  check_anapo_inspector_data_table <- dataframe4
  check_anapo_inspector_data_table <- dplyr::inner_join(check_anapo_inspector_data_table, check_anapo_inspector_dataplot_trip[, c("trip_id", "activity_id", "activity_date", "activity_number", "grounding", "activity_position")], by = dplyr::join_by(activity_id))
  check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
    dplyr::mutate(activity_position_prior = replace(activity_position, grounding, NA)) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(activity_date, activity_number) %>%
    tidyr::fill(activity_position_prior, .direction = "down") %>%
    dplyr::mutate(activity_position_prior = dplyr::lag(activity_position_prior))
  check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
    dplyr::mutate(activity_position_post = replace(activity_position, grounding, NA)) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::arrange(activity_date, activity_number) %>%
    tidyr::fill(activity_position_post, .direction = "up") %>%
    dplyr::mutate(activity_position_post = dplyr::lead(activity_position_post))
  check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("trip_id", "activity_date", "activity_number"))
  check_anapo_inspector_data_table$min_distance <- trunc(check_anapo_inspector_data_table$min_distance * 1000) / 1000
  check_anapo_inspector_data_table$max_score <- trunc(check_anapo_inspector_data_table$max_score * 1000) / 1000
  return(check_anapo_inspector_data_table)
}

#' @name plot_anapo_data
#' @title Function to create list data/argument for the plot plot_anapo
#' @description Function to create list data/argument
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_anapo_inspector}}, which must be done before using the plot_anapo_data () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_anapo_data () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_anapo_data () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#'  \item{\code{  vms_date}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  distance}}
#'  \item{\code{  duration}}
#'  \item{\code{  score}}
#'  \item{\code{  vms_crs}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  trip_enddate}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_number}}
#'  \item{\code{  vesselactivity_code}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoyoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{list}} with the sub list is a data for one plot
#' @doctest
#' dataframe1 <- data.frame(activity_id = c("4", "4", "4", "6", "6", "6", "2", "2", "2"),
#'                          activity_date = as.Date(c("2020/01/13", "2020/01/13", "2020/01/13",
#'                                                    "2020/01/13", "2020/01/13", "2020/01/13",
#'                                                    "2020/01/12", "2020/01/12", "2020/01/12")),
#'                          activity_time = c("03:12:34", "03:12:34", "03:12:34", "23:26:47",
#'                                            "23:26:47", "23:26:47", "05:26:01", "05:26:01",
#'                                            "05:26:01"),
#'                          activity_position = c("POINT (2.6 3.8)", "POINT (2.6 3.8)",
#'                                                "POINT (2.6 3.8)", "POINT (3 0.6)",
#'                                                "POINT (3 0.6)", "POINT (3 0.6)",
#'                                                "POINT (0 0)", "POINT (0 0)", "POINT (0 0)"),
#'                          activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326),
#'                          vms_date = as.Date(c("2020/01/13", "2020/01/13", "2020/01/12",
#'                                               "2020/01/13", "2020/01/13", "2020/01/12",
#'                                               "2020/01/12", "2020/01/13", "2020/01/13")),
#'                          vms_time = c("10:55:15", "22:32:17", "15:26:01", "10:55:15",
#'                                       "22:32:17", "15:26:01", "15:26:01", "10:55:15", "22:32:17"),
#'                          vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)",
#'                                           "POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)",
#'                                           "POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"),
#'                          distance = c(271.445826973379, 211.507663354955, 84.71741855987,
#'                                       182.602328607533, 18.012165417519, 212.768118823317,
#'                                       339.502583, 6.004055, 181.019203),
#'                          duration = c(-27761000, -69583000, -44007000, 45092000, 3270000,
#'                                       28846000, NA, NA, NA),
#'                          score = c(0, 0, 0, 0, 0.209441144555651, 0, NA, NA, NA),
#'                          vms_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326))
#' units::install_unit("NM", "1852 m", "Nautical mile")
#' dataframe1$distance <- units::set_units(dataframe1$distance, NM)
#' units::install_unit("ms", "1000 secs", "Milliseconds")
#' dataframe1$duration <- units::set_units(dataframe1$duration, ms)
#' dataframe2 <- data.frame(trip_id  = c("1", "1", "1"),
#'                          vessel_code = c("0", "0", "0"),
#'                          trip_enddate = as.Date(c("2020/01/20", "2020/01/20", "2020/01/20")),
#'                          activity_id = c("2", "4", "6"),
#'                          activity_date = as.Date(c("2020/01/12", "2020/01/13", "2020/01/13")),
#'                          activity_number = c(1L, 1L, 1L),
#'                          vesselactivity_code = c("2","6", "13"))
#' dataframe3 <- data.frame(transmittingbuoy_id  = c("1"),
#'                          transmittingbuoyoperation_code = c("4"),
#'                          activity_id = c("6"))
#' @expect equal(., list(`2` = list(data_trip = structure(list(activity_date = structure(c(18273, 18274, 18274), class = "Date"), activity_time = c("05:26:01", "03:12:34", "23:26:47"), activity_position = c("POINT (0 0)", "POINT (2.6 3.8)", "POINT (3 0.6)"), activity_number = c(1L, 1L, 1L), grounding = c(FALSE, FALSE, TRUE), vesselactivity_code = c("2", "6", "13")), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), data_activity = structure(list(vessel_code = "0", trip_enddate = structure(18281, class = "Date"), activity_date = structure(18273, class = "Date"), activity_time = "05:26:01", activity_position = "POINT (0 0)", activity_number = 1L, grounding = FALSE, vesselactivity_code = "2", X = "E0°0'", Y = "N0°0'"), class = "data.frame", row.names = 3L), data_vms = structure(list(vms_position = c("POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"), vms_date = structure(c(18273, 18274, 18274), class = "Date"), vms_time = c("15:26:01", "10:55:15", "22:32:17"), distance = structure(c(339.502583, 6.004055, 181.019203), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(NA_real_, NA_real_, NA_real_), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(NA_real_, NA_real_, NA_real_)), class = "data.frame", row.names = 7:9), crs_activity = 4326, crs_vms = 4326), `4` = list(data_trip = structure(list(activity_date = structure(c(18274, 18274, 18273), class = "Date"), activity_time = c("03:12:34", "23:26:47", "05:26:01"), activity_position = c("POINT (2.6 3.8)", "POINT (3 0.6)", "POINT (0 0)"), activity_number = c(1L, 1L, 1L), grounding = c(FALSE, TRUE, FALSE), vesselactivity_code = c("6", "13", "2")), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), data_activity = structure(list(vessel_code = "0", trip_enddate = structure(18281, class = "Date"), activity_date = structure(18274, class = "Date"), activity_time = "03:12:34", activity_position = "POINT (2.6 3.8)", activity_number = 1L, grounding = FALSE, vesselactivity_code = "6", X = "E2°36'", Y = "N3°48'"), class = "data.frame", row.names = 1L), data_vms = structure(list(vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)"), vms_date = structure(c(18274, 18274, 18273), class = "Date"), vms_time = c("10:55:15", "22:32:17", "15:26:01"), distance = structure(c(271.445826973379, 211.507663354955, 84.71741855987), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(-27761000, -69583000, -44007000), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(0, 0, 0)), class = "data.frame", row.names = c(NA, 3L)), crs_activity = 4326, crs_vms = 4326), `6` = list(data_trip = structure(list(activity_date = structure(c(18274, 18274, 18273), class = "Date"), activity_time = c("03:12:34", "23:26:47", "05:26:01"), activity_position = c("POINT (2.6 3.8)", "POINT (3 0.6)", "POINT (0 0)"), activity_number = c(1L, 1L, 1L), grounding = c(FALSE, TRUE, FALSE), vesselactivity_code = c("6", "13", "2")), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")), data_activity = structure(list(vessel_code = "0", trip_enddate = structure(18281, class = "Date"), activity_date = structure(18274, class = "Date"), activity_time = "23:26:47", activity_position = "POINT (3 0.6)", activity_number = 1L, grounding = TRUE, vesselactivity_code = "13", X = "E3°0'", Y = "N0°36'"), class = "data.frame", row.names = 2L), data_vms = structure(list(vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)"), vms_date = structure(c(18274, 18274, 18273), class = "Date"), vms_time = c("10:55:15", "22:32:17", "15:26:01"), distance = structure(c(182.602328607533, 18.012165417519, 212.768118823317), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(45092000, 3270000, 28846000), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(0, 0.209441144555651, 0)), class = "data.frame", row.names = 4:6), crs_activity = 4326, crs_vms = 4326)))
#' plot_anapo_data(dataframe1, dataframe2, dataframe3)
#' @export
plot_anapo_data <- function(dataframe1, dataframe2, dataframe3) {
  # 0 - Global variables assignement ----
  . <- NULL
  activity_id <- NULL
  trip_id <- NULL
  activity_date <- NULL
  activity_position <- NULL
  date_group <- NULL
  X <- NULL
  Y <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "activity_crs", "vms_date", "vms_time", "vms_position", "distance", "duration", "score", "vms_crs"),
    column_type = c("character", "Date", "character", "character", "numeric", "Date", "character", "character", "units", "units", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "activity_crs", "vms_date", "vms_time", "vms_position", "distance", "duration", "score", "vms_crs"),
      column_type = c("character", "Date", "character", "character", "numeric", "Date", "character", "character", "units", "units", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_date", "activity_time", "activity_position", "activity_crs", "vms_date", "vms_time", "vms_position", "distance", "duration", "score", "vms_crs")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_date", "activity_number", "vesselactivity_code"),
    column_type = c("character", "Date", "character", "character", "Date", "integer", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_date", "activity_number", "vesselactivity_code"),
      column_type = c("character", "Date", "character", "character", "Date", "integer", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_date", "activity_number", "vesselactivity_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("transmittingbuoy_id", "transmittingbuoyoperation_code", "activity_id")]
  }
  # 2 - Data design ----
  check_anapo_inspector_dataplot <- dplyr::inner_join(dataframe1, dataframe2[, c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_number", "vesselactivity_code")], by = dplyr::join_by(activity_id))
  # Add information on whether the activity is linked to a grounding (object or buoy) or not in data plot
  data_tmp_grounding <- column_grounding(data = check_anapo_inspector_dataplot, data_transmittingbuoy = dataframe3)
  check_anapo_inspector_dataplot <- dplyr::inner_join(check_anapo_inspector_dataplot, data_tmp_grounding, by = dplyr::join_by(activity_id))
  # Selecting useful data for the plot
  check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot %>%
    dplyr::select("trip_id", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "vesselactivity_code", "activity_crs", "grounding") %>%
    dplyr::group_by(trip_id) %>%
    dplyr::distinct()
  # Retrieves activity positions for the previous, current and next day
  check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot_trip %>%
    dplyr::select(-c("activity_id")) %>%
    dplyr::mutate(date_group = activity_date)
  check_anapo_inspector_dataplot_trip_prior <- check_anapo_inspector_dataplot_trip %>% dplyr::mutate(date_group = activity_date - 1)
  check_anapo_inspector_dataplot_trip_post <- check_anapo_inspector_dataplot_trip %>% dplyr::mutate(date_group = activity_date + 1)
  check_anapo_inspector_dataplot_range_date <- dplyr::bind_rows(check_anapo_inspector_dataplot_trip, check_anapo_inspector_dataplot_trip_prior, check_anapo_inspector_dataplot_trip_post) %>%
    dplyr::group_by(date_group, trip_id) %>%
    dplyr::distinct()
  check_anapo_inspector_dataplot_range_date <- dplyr::inner_join(check_anapo_inspector_dataplot_range_date, dataframe2[, c("activity_date", "trip_id", "activity_id")], by = dplyr::join_by(date_group == activity_date, trip_id == trip_id), relationship = "many-to-many")
  check_anapo_inspector_dataplot_range_date <- check_anapo_inspector_dataplot_range_date %>%
    dplyr::group_by(date_group, trip_id, activity_id) %>%
    dplyr::distinct()
  check_anapo_inspector_dataplot_range_date <- data_to_list(data = check_anapo_inspector_dataplot_range_date, name_col_dataplot = "data_trip", colname_id = "activity_id", colname_plot = c("activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code"), colname_info = NULL, rename_colname_info = NULL)
  # Data formatting controlled activity
  check_anapo_inspector_dataplot_activity <- check_anapo_inspector_dataplot %>%
    dplyr::select(c("vessel_code", "trip_enddate", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code")) %>%
    dplyr::distinct()
  # Retrieves X, Y coordinates of position
  check_anapo_inspector_dataplot_activity_geo <- check_anapo_inspector_dataplot_activity %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(wkt = "activity_position", crs = "4326", remove = FALSE) %>%
    dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
    dplyr::mutate(X = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  check_anapo_inspector_dataplot_activity <- dplyr::left_join(check_anapo_inspector_dataplot_activity, check_anapo_inspector_dataplot_activity_geo[, c("activity_id", "X", "Y")], by = dplyr::join_by(activity_id))
  check_anapo_inspector_dataplot_activity <- data_to_list(data = check_anapo_inspector_dataplot_activity, name_col_dataplot = "data_activity", colname_id = "activity_id", colname_plot = c("vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code", "X", "Y"), colname_info = NULL, rename_colname_info = NULL)
  check_anapo_inspector_dataplot <- check_anapo_inspector_dataplot %>%
    dplyr::select(-c("vessel_code", "trip_enddate", "activity_number", "activity_time", "vesselactivity_code"))
  check_anapo_inspector_dataplot <- data_to_list(data = check_anapo_inspector_dataplot, name_col_dataplot = "data_vms", colname_id = "activity_id", colname_plot = c("vms_position", "vms_date", "vms_time", "distance", "duration", "score"), colname_info = c("activity_crs", "vms_crs"), rename_colname_info = c("crs_activity", "crs_vms"))
  # Merge the various lists containing inofrmation useful for the plot
  if (length(check_anapo_inspector_dataplot_range_date) == length(check_anapo_inspector_dataplot_activity) && length(check_anapo_inspector_dataplot_range_date) == length(check_anapo_inspector_dataplot) && all(names(check_anapo_inspector_dataplot_range_date) == names(check_anapo_inspector_dataplot_activity)) && all(names(check_anapo_inspector_dataplot_range_date) == names(check_anapo_inspector_dataplot))) {
    check_anapo_inspector_dataplot <- Map(c, check_anapo_inspector_dataplot_range_date, check_anapo_inspector_dataplot_activity, check_anapo_inspector_dataplot)
  } else {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Problems when building the different lists for the Anapo plots.\n",
      "See next identifiers : ",
      paste0(names(table(c(names(check_anapo_inspector_dataplot_range_date), names(check_anapo_inspector_dataplot_activity), names(check_anapo_inspector_dataplot)))[table(c(names(check_anapo_inspector_dataplot_range_date), names(check_anapo_inspector_dataplot_activity), names(check_anapo_inspector_dataplot))) != 3]), collapse = ", "),
      sep = ""
    )
  }
  return(check_anapo_inspector_dataplot)
}

#' @name plot_anapo
#' @title Function to create the plot of the consistency of the position for the activity and VMS
#' @description Function to create the plot
#' @param data_vms {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_anapo_data}}, which must be done before using the plot_anapo () function.
#' @param crs_vms {\link[base]{numeric}} expected. Coordinates Reference System for position VMS
#' @param crs_activity {\link[base]{numeric}} expected. Coordinates Reference System for position activity
#' @param data_activity {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_anapo_data}}, which must be done before using the plot_anapo () function.
#' @param data_trip {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_anapo_data}}, which must be done before using the plot_anapo () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data_vms :
#'  \item{\code{  vms_date}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  distance}}
#'  \item{\code{  duration}}
#'  \item{\code{  score}}
#' }
#' \itemize{
#' data_activity :
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_number}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  grounding}}
#' }
#' \itemize{
#' data_trip :
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_number}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  grounding}}
#' }
#' @return The function returns a {\link[plotly]{plotly}}
#' # Example 1
#' data_vms <- data.frame(vms_date = as.Date(c("2020/01/12", "2020/01/13", "2020/01/13")),
#'                        vms_time = c("15:26:01", "10:55:15", "22:32:17"),
#'                        vms_position = c("POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"),
#'                        distance = c(339.502583, 6.004055, 181.019203),
#'                        duration = c(NA, NA, NA),
#'                        score = c(NA_real_, NA_real_, NA_real_))
#' units::install_unit("NM", "1852 m", "Nautical mile")
#' data_vms$distance <- units::set_units(data_vms$distance, NM)
#' units::install_unit("ms", "1000 secs", "Milliseconds")
#' data_vms$duration <- units::set_units(data_vms$duration, ms)
#' data_activity <- data.frame(activity_date =  as.Date(c("2020/01/12")),
#'                             activity_number = 1L,
#'                             activity_position = "POINT (0 0)",
#'                             vesselactivity_code = "2",
#'                             grounding = FALSE)
#' data_trip <- data.frame(activity_date = as.Date(c("2020/01/12", "2020/01/13", "2020/01/13")),
#'                         activity_time = c("05:26:01", "03:12:34", "23:26:47"),
#'                         activity_number = c(1L, 1L, 1L),
#'                         activity_position = c("POINT (0 0)", "POINT (2.6 3.8)", "POINT (3 0.6)"),
#'                         vesselactivity_code = c("2", "6", "13"),
#'                         grounding = c(FALSE,FALSE, TRUE))
#' plot_anapo(data_vms, 4326, 4326, data_activity, data_trip)
#' @export
plot_anapo <- function(data_vms, crs_vms, crs_activity, data_activity, data_trip) {
  # 0 - Global variables assignement ----
  . <- NULL
  vms_time <- NULL
  activity_date <- NULL
  activity_time <- NULL
  distance <- NULL
  duration <- NULL
  score <- NULL
  activity_number <- NULL
  vms_date <- NULL
  vesselactivity_code <- NULL
  grounding <- NULL
  vms_time_bis <- NULL
  activity_time_bis <- NULL
  date_time <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data_vms,
    type = "data.frame",
    column_name = c("vms_position", "vms_time", "vms_date", "distance", "duration", "score"),
    column_type = c("character", "character", "Date", "units", "units", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_vms,
      type = "data.frame",
      column_name = c("vms_position", "vms_time", "vms_date", "distance", "duration", "score"),
      column_type = c("character", "character", "Date", "units", "units", "numeric"),
      output = "error"
    )
  } else {
    data_vms <- data_vms[, c("vms_position", "vms_time", "vms_date", "distance", "duration", "score")]
  }
  if (!codama::r_type_checking(
    r_object = crs_vms,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = crs_vms,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = crs_activity,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = crs_activity,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_table_checking(
    r_table = data_activity,
    type = "data.frame",
    column_name = c("activity_position", "activity_date", "activity_number", "vesselactivity_code", "grounding"),
    column_type = c("character", "Date", "integer", "character", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_activity,
      type = "data.frame",
      column_name = c("activity_position", "activity_date", "activity_number", "vesselactivity_code", "grounding"),
      column_type = c("character", "Date", "integer", "character", "logical"),
      output = "error"
    )
  } else {
    data_activity <- data_activity[, c("activity_position", "activity_date", "activity_number", "vesselactivity_code", "grounding")]
  }
  if (!codama::r_table_checking(
    r_table = data_trip,
    type = "data.frame",
    column_name = c("activity_position", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "grounding"),
    column_type = c("character", "Date", "character", "integer", "character", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_trip,
      type = "data.frame",
      column_name = c("activity_position", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "grounding"),
      column_type = c("character", "Date", "character", "integer", "character", "logical"),
      output = "error"
    )
  } else {
    data_trip <- data_trip[, c("activity_position", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "grounding")]
  }
  # 2 - Data design ----
  # Remove missing position in vms
  data_vms <- data_vms %>% dplyr::filter(!is.na(data_vms$vms_position))
  # Format date time and order
  if (!all(is.na(data_vms$vms_position))) {
    # Gives a temporary hour for VMS that are missing an hour
    data_vms$vms_time_bis <- data_vms$vms_time
    data_vms[is.na(data_vms$vms_time), "vms_time_bis"] <- "00:00:00"
    data_vms <- data_vms %>%
      dplyr::mutate(date_time = as.POSIXct(paste(vms_date, vms_time_bis))) %>%
      dplyr::arrange(date_time) %>%
      dplyr::select(!c(vms_time_bis))
  }
  # Gives a temporary hour for activities that are missing an hour
  data_trip$activity_time_bis <- data_trip$activity_time
  data_trip[is.na(data_trip$activity_time), "activity_time_bis"] <- "00:00:00"
  data_trip <- data_trip %>%
    dplyr::mutate(date_time = as.POSIXct(paste(activity_date, activity_time_bis))) %>%
    dplyr::arrange(date_time) %>%
    dplyr::select(!c(activity_time_bis))
  # Spatial formatting
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_vms[!is.na(data_vms$vms_position), ] %>%
      sf::st_as_sf(wkt = "vms_position", crs = as.numeric(crs_vms)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  }
  if (!all(is.na(data_activity$activity_position))) {
    data_geo_activity <- data_activity[!is.na(data_activity$activity_position), ] %>%
      sf::st_as_sf(wkt = "activity_position", crs = as.numeric(crs_activity)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  }
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip <- data_trip[!is.na(data_trip$activity_position), ] %>%
      sf::st_as_sf(wkt = "activity_position", crs = as.numeric(crs_activity)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  }
  # text hovertemplate
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_geo_vms %>% dplyr::mutate(text = paste("Date:", vms_date, "<br>Time:", vms_time, "<br>Distance:", trunc(distance * 1000) / 1000, "miles<br>Duration:", trunc((duration / 60000) * 1000) / 1000, "minutes<br>Score:", trunc(score * 1000) / 1000, "<br>Position:", latitude, ",", longitude, "<extra></extra>"))
  }
  if (!all(is.na(data_activity$activity_position))) {
    data_geo_activity <- data_geo_activity %>% dplyr::mutate(text = paste("Date:", activity_date, "<br>Time:", activity_time, "<br>Activity number:", activity_number, "<br>Vessel activity:", vesselactivity_code, "<br>Position:", latitude, ",", longitude, "<br>Grounding:", grounding, "<extra></extra>"))
  }
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip <- data_geo_trip %>% dplyr::mutate(text = paste("Date:", activity_date, "<br>Time:", activity_time, "<br>Activity number:", activity_number, "<br>Vessel activity:", vesselactivity_code, "<br>Grounding:", grounding, "<br>Position:", latitude, ",", longitude, "<extra></extra>"))
  }
  # 3 - Plot ----
  plot <- plotly::plot_ly() %>%
    plotly::layout(mapbox = list(style = "carto-positron", pitch = 0, zoom = 6))
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip_grounding <- data_geo_trip %>% dplyr::filter(grounding)
    data_geo_trip_nongrounding <- data_geo_trip %>% dplyr::filter(!grounding)
    plot <- plot %>%
      plotly::add_trace(name = "Activity (solely grounding object)", data = data_geo_trip_grounding, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", hovertemplate = ~text, marker = list(color = "rgb(142, 52, 0)", size = 10))
    plot <- plot %>%
      plotly::add_trace(name = "Activity", data = data_geo_trip_nongrounding, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", hovertemplate = ~text, marker = list(color = grDevices::colorRampPalette(c("#B2FF00", "#006415"))(nrow(data_geo_trip_nongrounding)), size = 10), line = list(color = "#11BC00"))
  }
  if (!all(is.na(data_vms$vms_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "VMS", data = data_geo_vms, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", hovertemplate = ~text, marker = list(color = grDevices::colorRampPalette(c("#00F7FF", "#3B18AA"))(nrow(data_geo_vms)), size = 10), line = list(color = "#0032FF")) %>%
      plotly::layout(mapbox = list(center = list(lon = data_geo_vms$X[1], lat = data_geo_vms$Y[1])))
  }
  if (!all(is.na(data_activity$activity_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "Suspicious activity", data = data_geo_activity, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", hovertemplate = ~text, marker = list(color = "rgb(255, 0, 0)", size = 10)) %>%
      plotly::layout(mapbox = list(center = list(lon = data_geo_activity$X, lat = data_geo_activity$Y)))
  }
  return(plot)
}

#' @name plot_anapo_windows
#' @title Function to create the text for windows with plot anapo
#' @description Function to create the windows with plot of the consistency of the position for the activity and VMS
#' @param data_activity {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_anapo_data}}, which must be done before using the plot_anapo () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data_activity :
#'  \item{\code{  vessel_code}}
#'  \item{\code{  trip_enddate}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_number}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  X}}
#'  \item{\code{  Y}}
#'  \item{\code{  grounding}}
#' }
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' data_activity <- data.frame(vessel_code = c("0"),
#'                             trip_enddate = as.Date(c("2020/01/20")),
#'                             activity_date =  as.Date(c("2020/01/12")),
#'                             activity_time = c("05:26:01"),
#'                             activity_number = 1L,
#'                             vesselactivity_code = "2",
#'                             X = c("E0°0'"),
#'                             Y = c("N0°0'"),
#'                             grounding = FALSE)
#' @expect equal(., "<b>Trip information : </b><br>\n         <ul><li>Vessel code : 0</li>\n         <li>Trip end date : 2020-01-20</li>\n         <li>Activity date : 2020-01-12</li>\n         <li>Activity time : 05:26:01</li>\n         <li>Activity number : 1</li>\n         <li>Vessel activity : 2</li>\n         <li>Latitude : N0°0'</li>\n         <li>Longitude : E0°0'</li>\n         <li>Grounding : FALSE</li></ul>")
#' plot_anapo_windows(data_activity)
#' @export
plot_anapo_windows <- function(data_activity) {
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data_activity,
    type = "data.frame",
    column_name = c("vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "Y", "X", "grounding"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_activity,
      type = "data.frame",
      column_name = c("vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "Y", "X", "grounding"),
      output = "error"
    )
  } else {
    data_activity <- data_activity[, c("vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_number", "vesselactivity_code", "Y", "X", "grounding")]
  }
  # 2 - Data design ----
  paste0("<b>Trip information : </b><br>
         <ul><li>Vessel code : ", data_activity[["vessel_code"]], "</li>
         <li>Trip end date : ", data_activity[["trip_enddate"]], "</li>
         <li>Activity date : ", data_activity[["activity_date"]], "</li>
         <li>Activity time : ", data_activity[["activity_time"]], "</li>
         <li>Activity number : ", data_activity[["activity_number"]], "</li>
         <li>Vessel activity : ", data_activity[["vesselactivity_code"]], "</li>
         <li>Latitude : ", data_activity[["Y"]], "</li>
         <li>Longitude : ", data_activity[["X"]], "</li>
         <li>Grounding : ", data_activity[["grounding"]], "</li></ul>")
}

#' @name plot_anapo_activity_data
#' @title Function to create list data/argument for the plot plot_anapo_activity
#' @description Function to create list data/argument
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or the second output of the function {\link[codama]{check_anapo_activity_consistent_inspector}}, which must be done before using the plot_anapo_activity_data () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the plot_anapo_activity_data () function.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  vms_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  vms_date}}
#'  \item{\code{  vessel_type}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  vms_id}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  vms_crs}}
#' }
#' @return The function returns a {\link[base]{list}} with the sub list is a data for one plot
#' @doctest
#' dataframe1 <- data.frame(vms_id = c("1", "2"),
#'                          vessel_code = c("0", "0"),
#'                          vms_date = as.Date(c("2020/01/13", "2020/01/13")),
#'                          vessel_type = c("6", "6"))
#' dataframe2 <- data.frame(vms_id = c("1", "2"),
#'                          vms_time = c("10:55:15", "22:32:17"),
#'                          vms_position = c("POINT (0 0.1)", "POINT (3 0.3)"),
#'                          vms_crs = c(4326, 4326))
#' @expect equal(., list(`1` = list(data_vms = structure(list(vms_position = "POINT (0 0.1)", vms_time = "10:55:15"), class = "data.frame", row.names = 1L), date_vms = structure(18274, class = "Date"), crs_vms = 4326, vessel_code = "0", vessel_type = "6"), `2` = list(data_vms = structure(list(vms_position = "POINT (3 0.3)", vms_time = "22:32:17"), class = "data.frame", row.names = 2L), date_vms = structure(18274, class = "Date"), crs_vms = 4326, vessel_code = "0", vessel_type = "6")))
#' plot_anapo_activity_data(dataframe1, dataframe2)
#' @export
plot_anapo_activity_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  vms_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("vms_id", "vessel_code", "vms_date", "vessel_type"),
    column_type = c("character", "character", "Date", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("vms_id", "vessel_code", "vms_date", "vessel_type"),
      column_type = c("character", "character", "Date", "character"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("vms_id", "vms_time", "vms_position", "vms_crs"),
    column_type = c("character", "character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("vms_id", "vms_time", "vms_position", "vms_crs"),
      column_type = c("character", "character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("vms_id", "vms_time", "vms_position", "vms_crs")]
  }
  # 2 - Data design ----
  # Retrieving information for the plot
  check_anapo_activity_dataplot <- dplyr::inner_join(dataframe1, dataframe2[, c("vms_id", "vms_time", "vms_position", "vms_crs")], by = dplyr::join_by(vms_id))
  check_anapo_activity_dataplot <- data_to_list(data = check_anapo_activity_dataplot, name_col_dataplot = "data_vms", colname_id = "vms_id", colname_plot = c("vms_position", "vms_time"), colname_info = c("vms_date", "vms_crs", "vessel_code", "vessel_type"), rename_colname_info = c("date_vms", "crs_vms", "vessel_code", "vessel_type"))
  return(check_anapo_activity_dataplot)
}

#' @name plot_anapo_activity
#' @title Function to create the plot of the consistency of the position for VMS
#' @description Function to create the plot
#' @param data_vms {\link[base]{data.frame}} expected. Csv or an element of the output of function {\link[AkadoR]{plot_anapo_activity_data}}, which must be done before using the plot_anapo_activity () function.
#'@param crs_vms {\link[base]{numeric}} expected. Coordinates Reference System for position VMS
#' @param date_vms {\link[base]{Date}} expected. VMS date
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data_vms :
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#' }
#' @return The function returns a {\link[plotly]{plotly}}
#' # Example 1
#' data_vms <- data.frame(vms_time = c("10:55:15", "22:32:17"),
#'                        vms_position = c("POINT (0 0.1)", "POINT (3 0.3)"))
#' plot_anapo_activity(data_vms, 4326, as.Date(c("2020/01/13")))
#' @export
plot_anapo_activity <- function(data_vms, crs_vms, date_vms) {
  # 0 - Global variables assignement ----
  . <- NULL
  vms_time <- NULL
  vms_time_bis <- NULL
  date_time <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data_vms,
    type = "data.frame",
    column_name = c("vms_position", "vms_time"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_vms,
      type = "data.frame",
      column_name = c("vms_position", "vms_time"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    data_vms <- data_vms[, c("vms_position", "vms_time")]
  }
  if (!codama::r_type_checking(
    r_object = crs_vms,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = crs_vms,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = date_vms,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = date_vms,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Remove missing position in vms
  data_vms <- data_vms %>% dplyr::filter(!is.na(data_vms$vms_position))
  # Format date time and order
  if (!all(is.na(data_vms$vms_position))) {
    # Gives a temporary hour for VMS that are missing an hour
    data_vms$vms_time_bis <- data_vms$vms_time
    data_vms[is.na(data_vms$vms_time), "vms_time_bis"] <- "00:00:00"
    data_vms <- data_vms %>%
      dplyr::mutate(date_time = as.POSIXct(paste(date_vms, vms_time_bis))) %>%
      dplyr::arrange(date_time) %>%
      dplyr::select(!c(vms_time_bis))
  }
  # Spatial formatting
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_vms[!is.na(data_vms$vms_position), ] %>%
      sf::st_as_sf(wkt = "vms_position", crs = as.numeric(crs_vms)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
  }
  # text hovertemplate
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_geo_vms %>% dplyr::mutate(text = paste("Date:", date_vms, "<br>Time:", vms_time, "<br>Position:", latitude, ",", longitude, "<extra></extra>"))
  }
  # 3 - Plot ----
  plot <- plotly::plot_ly() %>%
    plotly::layout(mapbox = list(style = "carto-positron", pitch = 0, zoom = 6))
  if (!all(is.na(data_vms$vms_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "VMS", data = data_geo_vms, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", hovertemplate = ~text, marker = list(color = grDevices::colorRampPalette(c("#00F7FF", "#3B18AA"))(nrow(data_geo_vms)), size = 10), line = list(color = "#0032FF")) %>%
      plotly::layout(showlegend = TRUE, mapbox = list(center = list(lon = data_geo_vms$X[1], lat = data_geo_vms$Y[1])))
  }
  return(plot)
}

#' @name plot_anapo_activity_windows
#' @title Function to create the text for windows with plot anapo activity
#' @description Function to create the windows with plot of the consistency of the position for VMS
#' @param vessel_code {\link[base]{character}} expected. Vessel code
#' @param date_vms {\link[base]{Date}} expected. VMS date
#' @param vessel_type {\link[base]{character}} expected. Vessel type
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., "<b>Trip information : </b><br>\n         <ul><li>Vessel code : 0</li>\n         <li>VMS date : 2020-01-13</li>\n         <li>Vessel type : 6</li></ul>")
#' plot_anapo_activity_windows("0", as.Date(c("2020/01/13")), "6")
#' @export
plot_anapo_activity_windows <- function(vessel_code, date_vms, vessel_type) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = vessel_code,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_code,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = date_vms,
    type = "Date",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = date_vms,
      type = "Date",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_type,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  paste0("<b>Trip information : </b><br>
         <ul><li>Vessel code : ", vessel_code, "</li>
         <li>VMS date : ", date_vms, "</li>
         <li>Vessel type : ", vessel_type, "</li></ul>")
}
