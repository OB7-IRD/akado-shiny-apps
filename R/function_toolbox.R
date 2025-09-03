
#' @name data_to_list
#' @title Function to create a data.frame in list
#' @description Function to create a data.frame in list
#' @param data {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the data_to_list () function.
#' @param name_col_dataplot {\link[base]{character}} expected. Name of the sub-list containing the table
#' @param colname_id {\link[base]{character}} expected. Name of the column in data used to identify a group
#' @param colname_plot {\link[base]{character}} expected. Vector of column names in data to be grouped in an array because it contains several values per group
#' @param colname_info {\link[base]{character}} expected. Vector of column names in data to be organized in separate sub-lists because it contains a single value per group
#' @param rename_colname_info {\link[base]{character}} expected. Vector with sub-list names to be renamed
#' @return The function returns a {\link[base]{list}} with the sub list
#' @doctest
#' dataframe1 <- data.frame(trip_id = c("1", "1", "2", "2", "2"),
#'                          trip_startdate = as.Date(c("2020/01/01", "2020/01/01", "2020/01/01",
#'                                                     "2020/01/01", "2020/01/01")),
#'                          trip_enddate = as.Date(c("2020/01/02", "2020/01/02", "2020/01/02",
#'                                                   "2020/01/02", "2020/01/02")),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/02","2020/01/01",
#'                                                    "2020/01/02", "2020/01/03")),
#'                          logical = c(TRUE, TRUE, TRUE, TRUE, FALSE))
#' @expect equal(., list(`1` = list(data = structure(list(activity_date = structure(c(18262, 18263), class = "Date"), logical = c(TRUE, TRUE)), class = "data.frame", row.names = 1:2), startdate = structure(18262, class = "Date"), enddate = structure(18263, class = "Date")), `2` = list(data = structure(list(activity_date = structure(c(18262, 18263, 18264), class = "Date"), logical = c(TRUE, TRUE, FALSE)), class = "data.frame", row.names = 3:5), startdate = structure(18262, class = "Date"), enddate = structure(18263, class = "Date"))))
#' data_to_list(data = dataframe1, name_col_dataplot = "data", colname_id = "trip_id",
#'              colname_plot = c("activity_date", "logical"),
#'              colname_info = c("trip_startdate", "trip_enddate"),
#'              rename_colname_info = c("startdate", "enddate"))
#' @export
data_to_list <- function(data, name_col_dataplot, colname_id, colname_plot, colname_info, rename_colname_info) {
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c(colname_id, colname_plot, colname_info),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c(colname_id, colname_plot, colname_info),
      output = "error"
    )
  }
  if (!codama::r_type_checking(
    r_object = name_col_dataplot,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = name_col_dataplot,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = colname_id,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_id,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = colname_plot,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_plot,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = colname_info,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_info,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = rename_colname_info,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = rename_colname_info,
      type = "character",
      output = "error"
    ))
  }
  if (length(colname_info) != length(rename_colname_info)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"colname_info\" and \"name_col_infoplot\"\n"
    )
  }
  # 2 - Data design ----
  # Partition table by id
  data_split <- data %>%
    dplyr::ungroup() %>%
    dplyr::select(c(colname_id, colname_plot, colname_info)) %>%
    split(data[[colname_id]])
  # Lists useful plot information for each id
  data_list <- lapply(stats::setNames(data_split, names(data_split)), function(data_tmp) {
    list_tmp <- list()
    # Extracts the data frame used for the plot
    if (length(colname_plot) > 0) {
      list_tmp[[name_col_dataplot]] <-  data_tmp[, colname_plot]
    }
    # Extracts the value used for the plot
    if (length(colname_info) > 0) {
      list_tmp <- c(list_tmp, stats::setNames(lapply(colname_info, function(col) unique(data_tmp[[col]])), rename_colname_info))
    }
    return(list_tmp)
  })
  return(data_list)
}

#' @name column_grounding
#' @title Function detects activity linked solely to grounding
#' @description Function detects activity linked solely to grounding
#' @param data {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the column_grounding () function.
#' @param data_transmittingbuoy {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the column_grounding () function.
#' @param transmittingbuoyoperation_grounding_code {\link[base]{character}} expected. Default values: c("4", "5"). vector of buoy operation codes indicating object grounding
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' data :
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' data_transmittingbuoy :
#'  \item{\code{  transmittingbuoyoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{data.frame}}
#' @doctest
#' data <- data.frame(activity_id = c("1", "2", "3"))
#' data_transmittingbuoy <- data.frame(transmittingbuoyoperation_code = c("1", "4"),
#'                                     activity_id = c("1", "3"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3"), grounding = c(FALSE, FALSE, TRUE)), row.names = c(NA, -3L), class = "data.frame"))
#' column_grounding(data, data_transmittingbuoy)
#' @export
column_grounding <- function(data,
                             data_transmittingbuoy,
                             transmittingbuoyoperation_grounding_code = c("4", "5")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  transmittingbuoyoperation_code <- NULL
  all_grounding <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("activity_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("activity_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    data <- data[, c("activity_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = data_transmittingbuoy,
    type = "data.frame",
    column_name = c("activity_id", "transmittingbuoyoperation_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_transmittingbuoy,
      type = "data.frame",
      column_name = c("activity_id", "transmittingbuoyoperation_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    data_transmittingbuoy <- data_transmittingbuoy[, c("activity_id", "transmittingbuoyoperation_code")]
  }
  if (!codama::r_type_checking(
    r_object = transmittingbuoyoperation_grounding_code,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = transmittingbuoyoperation_grounding_code,
      type = "character",
      output = "error"
    ))
  }
  # 2 - Data design ----
  select <- data$activity_id
  nrow_first <- length(unique(select))
  data <- data %>% dplyr::distinct()
  data$grounding <- FALSE
  # Recovers activity linked solely to grounding
  activity_grounding_transmittingbuoy <- data_transmittingbuoy %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(all_grounding = sum(transmittingbuoyoperation_code %in% transmittingbuoyoperation_grounding_code) == dplyr::n()) %>%
    dplyr::filter(all_grounding)
  if (length(activity_grounding_transmittingbuoy) > 0) {
    comparison_transmittingbuoy <- codama::vector_comparison(
      first_vector = data$activity_id,
      second_vector = activity_grounding_transmittingbuoy$activity_id,
      comparison_type = "difference",
      output = "report"
    )
    data$grounding <- data$grounding | comparison_transmittingbuoy$logical
  }
  if ((sum(data$grounding, na.rm = TRUE) + sum(!data$grounding, na.rm = TRUE)) != nrow_first || any(is.na(data$grounding))) {
    all <- c(select, data$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(data$grounding))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(data$grounding)), "):", paste0(data$activity_id[is.na(data$grounding)], collapse = ", "))
    }
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      text,
      sep = ""
    )
  }
  # 3 - Export ----
  return(data)
}

#' @name text_object_more_or_less
#' @title Function to list elements with a number of occurrences other than 2
#' @description Function to list elements with a number of occurrences other than 2
#' @param id {\link[base]{character}} expected. Vector containing all desired identifiers at once
#' @param result_check {\link[base]{character}} expected. Vector containing the identifiers to be verified
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., "Missing item (1):2\nToo many item (1):3")
#' text_object_more_or_less(id = c("1", "2", "3"), result_check = c("1", "3", "3"))
#' @export
text_object_more_or_less <- function(id, result_check) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = id,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = id,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = result_check,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = result_check,
      type = "character",
      output = "error"
    ))
  }
  # 2 - Data design ----
  all <- c(id, result_check)
  number_occurrences <- table(all)
  text <- ""
  if (any(number_occurrences == 1)) {
    text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
  }
  if (any(number_occurrences > 2)) {
    text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
  }
  return(text)
}

#
#' @name coordinate_dd_to_dmd
#' @title Converting DD coordinates in DDM
#' @description Function for converting DD (Decimal Degrees) coordinates in DDM (Degrees, Decimal Minutes)
#' @param coordinate {\link[base]{numeric}} expected. Vector of coordinate values
#' @param latitude {\link[base]{logical}} expected. Logical which indicates whether the values correspond to the latitude (TRUE) of the coordinates or to the longitude (FALSE).
#' @return The function returns a {\link[base]{character}}
#' @doctest
#' @expect equal(., c("N0°6'", "N5°41.4'"))
#' coordinate_dd_to_dmd(coordinate = c(0.1, 5.69), latitude = TRUE)
#' @export
coordinate_dd_to_dmd <- function(coordinate, latitude) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = coordinate,
    type = "numeric",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = coordinate,
      type = "numeric",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = latitude,
    type = "logical",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = latitude,
      type = "logical",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Integral part of degrees
  degrees <- trunc(coordinate)
  # Decimal Minutes
  minutes_decimal <- round(abs(coordinate - degrees) * 60, 2)
  # Adds direction (N/S for latitude, E/W for longitude)
  direction <- rep(NA, length(coordinate))
  if (latitude) {
    direction[coordinate >= 0] <- "N"
    direction[coordinate < 0] <- "S"
  } else {
    direction[coordinate >= 0] <- "E"
    direction[coordinate < 0] <- "W"
  }
  paste0(direction, abs(degrees), "\u00B0", minutes_decimal, "'")
}
