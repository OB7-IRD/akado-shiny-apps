#' @name check_trip_activity_inspector
#' @title Gives the inconsistencies between the trip and the associated activities in terms of presence
#' @description The purpose of the check_trip_activity_inspector function is to provide a table of data that contains an inconsistency between the trip and the presence or not of activity associated with this trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_trip_activity_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_trip_activity_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 is not linked to an activity
#' dataframe1 <- data.frame(trip_id = c("1", "2"))
#' dataframe2 <- data.frame(trip_id = c("1"),
#'                          activity_id = c("1"))
#' @expect equal(., structure(list(trip_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' check_trip_activity_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_trip_activity_inspector <- function(dataframe1,
                                          dataframe2,
                                          output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "activity_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search trip ID in the associations trip, route, activity
  dataframe2 <- dataframe2[!is.na(dataframe2$activity_id), ]
  # Check
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_id,
    second_vector = dataframe2$trip_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trip with no activity")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_fishing_time_inspector
#' @title Gives the inconsistencies between the sum of the fishing times indicated for the route and the one indicated for the trip
#' @description The purpose of the check_fishing_time_inspector function is to provide a table of data that contains an inconsistency between the sum of the fishing times indicated for the route and the one indicated for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_time_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_time_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_fishingtime}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  route_fishingtime}}
#' }
#' @doctest
#' #Trip 1 and 5 are ok,
#' #Trip 2 has different fishing times,
#' #Trip 3 has no trip_fishingtime,
#' #Trip 4 has no route_fishingtime
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          trip_fishingtime = c(10, 15, NA, 8, 0))
#' dataframe2 <- data.frame(route_id = c("1", "2", "3", "4", "5", '6'),
#'                          trip_id = c("1", "1", "2", "2", "3", "5"),
#'                          route_fishingtime = c(4, 6, 10, 6, 14, 0))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, FALSE, FALSE, TRUE), trip_fishingtime = c(10, 15, NA, 8, 0), sum_route_fishingtime = c(10, 16, 14, NA, 0)), row.names = c(NA, -5L), class = "data.frame"))
#' check_fishing_time_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_fishing_time_inspector <- function(dataframe1,
                                         dataframe2,
                                         output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  route_fishingtime <- NULL
  trip_fishingtime <- NULL
  sum_route_fishingtime <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_fishingtime"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_fishingtime"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_fishingtime")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("route_id", "trip_id", "route_fishingtime"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("route_id", "trip_id", "route_fishingtime"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("route_id", "trip_id", "route_fishingtime")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the sum of the fishing time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_fishingtime = ifelse(all(is.na(route_fishingtime)), route_fishingtime[NA_integer_], sum(route_fishingtime, na.rm = TRUE)))
  # Group the pair to compare
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Compare fishing time of the trip or the sum of the route
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_fishingtime,
    second_vector = dataframe1$sum_route_fishingtime,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, trip_fishingtime, sum_route_fishingtime, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trip with a fishing time different from the sum of the fishing times of each activity")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_sea_time_inspector
#' @title Gives the inconsistencies between the sum of the sea times indicated for the route and the one indicated for the trip
#' @description The purpose of the check_sea_time_inspector function is to provide a table of data that contains an inconsistency between the sum of the sea times indicated for the route and the one indicated for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sea_time_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sea_time_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_seatime}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  route_seatime}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 has different sea times,
#' #Trip 3 has no trip_seatime,
#' #Trip 4 has no route_seatime,
#' #Trip 5 has trip_seatime = 0
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          trip_seatime = c(10, 15, NA, 8, 0))
#' dataframe2 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6"),
#'                          trip_id = c("1", "1", "2", "2", "3", "5"),
#'                          route_seatime = c(4, 6, 10, 6, 5, 0))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE), trip_seatime = c(10, 15, NA, 8, 0), sum_route_seatime = c(10, 16, 5, NA, 0)), row.names = c(NA, -5L), class = "data.frame"))
#' check_sea_time_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sea_time_inspector <- function(dataframe1,
                                     dataframe2,
                                     output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  route_seatime <- NULL
  trip_seatime <- NULL
  sum_route_seatime <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_seatime"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_seatime"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_seatime")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("route_id", "trip_id", "route_seatime"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("route_id", "trip_id", "route_seatime"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("route_id", "trip_id", "route_seatime")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the sum of the sea time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_seatime = ifelse(all(is.na(route_seatime)), route_seatime[NA_integer_], sum(route_seatime, na.rm = TRUE)))
  # Group the pair to compare
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Compare trip IDs and sea time of the trip or the sum of the route
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_seatime,
    second_vector = dataframe1$sum_route_seatime,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, trip_seatime, sum_route_seatime, .after = logical)
  # Management of the 0 value for the time at sea
  dataframe1[!is.na(dataframe1$trip_seatime) & dataframe1$trip_seatime == 0, "logical"] <- FALSE
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trip with a sea time different from the sum of the sea times of each activity")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' @name check_landing_consistent_inspector
#' @title Gives the inconsistencies between the total landed weight for canneries and local market in the trip and vessel capacity link to trip
#' @description The purpose of the check_landing_consistent_inspector function is to provide a table of data that contains an inconsistency with the total landed weight greater than vessel capacity for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_landing_consistent_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_landingtotalweight}}
#'  \item{\code{  trip_localmarkettotalweight}}
#'  \item{\code{  vessel_capacity}}
#' }
#' @doctest
#' #Trip 1 and 4 are ok,
#' #Trip 2 has a total weight greater than the vessel's capacity,
#' #Trip 3 has its vessel's capacity missing
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4"),
#'                          trip_landingtotalweight = c(10, 15, 2, NA),
#'                          trip_localmarkettotalweight = c(2, 1, 7, NA),
#'                          vessel_capacity = c(20, 10, NA, 13))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4"), logical = c(TRUE, FALSE, FALSE, TRUE), vessel_capacity = c(14, 7, NA, 9.1), trip_weighttotal = c(12, 16, 9, 0)), row.names = c(NA, 4L), class = "data.frame"))
#' check_landing_consistent_inspector(dataframe1, output = "report")
#' @export
check_landing_consistent_inspector <- function(dataframe1,
                                               output) {
  # 0 - Global variables assignement ----
  vessel_capacity <- NULL
  trip_weighttotal <- NULL
  trip_landingtotalweight <- NULL
  trip_localmarkettotalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1$trip_weighttotal <- rowSums(x = dataframe1[, c("trip_landingtotalweight", "trip_localmarkettotalweight")], na.rm = TRUE)
  # Converts cubic meters to tons
  dataframe1$vessel_capacity <- dataframe1$vessel_capacity * 0.7
  # Compare landing total weight of the trip with vessel capacity
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_weighttotal,
    second_vector = dataframe1$vessel_capacity,
    comparison_type = "less",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, vessel_capacity, trip_weighttotal, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(trip_landingtotalweight, trip_localmarkettotalweight))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with vessel capacity smaller than the landing total weight")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_landing_total_weight_inspector
#' @title Gives the inconsistencies between the total weight landed during the trip for the canneries and the sum of the weights of each landing for the canneries linked to the trip
#' @description The purpose of the check_landing_total_weight_inspector function is to provide a table of data that contains an inconsistency between the sum of the weights of each landing for the canneries and the one indicated for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_landing_total_weight_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_landing_total_weight_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_landingtotalweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  trip_id}}
#' }
#' @doctest
#' #Trip 1 and 4 are ok,
#' #Trip 2 has different landing weight,
#' #Trip 3 has no trip_landingtotalweight,
#' #Trip 5 has no landing_weight
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          trip_landingtotalweight = c(10, 15, NA, 0, 4))
#' dataframe2 <- data.frame(landing_id = c("1", "2", "3", "4", "5", "6"),
#'                          landing_weight = c(4, 6, 10, 6, 2, NA),
#'                          trip_id = c("1", "1", "2", "2", "3", "4"))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, FALSE, TRUE, FALSE), trip_landingtotalweight = c(10, 15, NA, 0, 4), sum_weightlanding = c(10, 16, 2, NA, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' check_landing_total_weight_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_landing_total_weight_inspector <- function(dataframe1,
                                                 dataframe2,
                                                 output,
                                                 epsilon = 0.01) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  landing_weight <- NULL
  trip_landingtotalweight <- NULL
  sum_weightlanding <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_landingtotalweight"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_landingtotalweight"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_landingtotalweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("landing_id", "landing_weight", "trip_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("landing_id", "landing_weight", "trip_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("landing_id", "landing_weight", "trip_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  # Checks the type of epsilon
  if (!codama::r_type_checking(
    r_object = epsilon,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = epsilon,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_weightlanding = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge and calcul difference
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  dataframe1$difference <- ifelse(is.na(dataframe1$trip_landingtotalweight), 0, dataframe1$trip_landingtotalweight) - ifelse(is.na(dataframe1$sum_weightlanding), 0, dataframe1$sum_weightlanding)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, trip_landingtotalweight, sum_weightlanding, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(difference, epsilon))
  # Management of missing landing weight in trip
  dataframe1[is.na(dataframe1$trip_landingtotalweight), "logical"] <- FALSE
  # Management of missing sum of the landing
  dataframe1[is.na(dataframe1$sum_weightlanding) & !is.na(dataframe1$trip_landingtotalweight) & dataframe1$trip_landingtotalweight > 0, "logical"] <- FALSE
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with a landing weight for the canneries different from the sum of the weights of each landing for the canneries")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_temporal_limit_inspector
#' @title Gives the inconsistencies between trip start and end date and the dates of activity
#' @description The purpose of the check_temporal_limit_inspector function is to provide a table of data that contains an inconsistency between trip start and end date and the dates of activity (activity date outside the trip ranges, several occurrences of the activity date, ...)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_temporal_limit_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_temporal_limit_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first at the trip level and the second at the activity date level), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_startdate}}
#'  \item{\code{  trip_enddate}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  trip_id}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 has an extra day (2020/01/03),
#' #Trip 3 has no trip_startdate,
#' #Trip 4 has no trip_enddate,
#' #Trip 5 has a missing day (2020/02/02),
#' #Trip 6 has a double day (2020/02/13)
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5", "6"),
#'                          trip_startdate = as.Date(c("2020/01/01", "2020/01/01", NA, "2020/01/24",
#'                                                     "2020/02/01", "2020/02/13")),
#'                          trip_enddate = as.Date(c("2020/01/02", "2020/01/02", "2020/01/24", NA,
#'                                                   "2020/02/03", "2020/02/14")))
#' dataframe2 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                       "12"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/02", "2020/01/01",
#'                                                    "2020/01/02", "2020/01/03", "2020/01/24",
#'                                                    "2020/01/24", "2020/02/01", "2020/02/03",
#'                                                    "2020/02/13", "2020/02/13", "2020/02/14")),
#'                          trip_id = c("1", "1", "2", "2", "2", "3", "4", "5", "5", "6", "6", "6"))
#' @expect equal(., list(structure(list(trip_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)), row.names = c(NA, -6L), class = "data.frame"), structure(list(trip_id = c("1", "1", "2", "2", "2", "3", "4", "5", "5", "6", "6"), trip_startdate = structure(c(18262, 18262, 18262, 18262, 18262, NA, 18285, 18293, 18293, 18305, 18305), class = "Date"), trip_enddate = structure(c(18263, 18263, 18263, 18263, 18263, 18285, NA, 18295, 18295, 18306, 18306), class = "Date"), activity_date = structure(c(18262, 18263, 18262, 18263, 18264, 18285, 18285, 18293, 18295, 18305, 18306), class = "Date"), inter_activity_date = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE), exter_activity_date = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), count_freq = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L), logical = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)), row.names = c(NA, -11L), class = "data.frame")))
#' check_temporal_limit_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_temporal_limit_inspector <- function(dataframe1,
                                           dataframe2,
                                           output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  trip_startdate <- NULL
  trip_enddate <- NULL
  activity_date <- NULL
  inter_activity_date <- NULL
  exter_activity_date <- NULL
  nb_day <- NULL
  logical_nb_day <- NULL
  logical_tmp <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_startdate", "trip_enddate"),
    column_type = c("character", "Date", "Date"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_startdate", "trip_enddate"),
      column_type = c("character", "Date", "Date"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_startdate", "trip_enddate")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("route_id", "activity_date", "trip_id"),
    column_type = c("character", "Date", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("route_id", "activity_date", "trip_id"),
      column_type = c("character", "Date", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("route_id", "activity_date", "trip_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge date
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Calculate the temporal indicator per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  trip_date_activity_data_detail <- dataframe1
  # Calculation if the date is in the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_greater_equal <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_startdate,
    comparison_type = "greater_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_less_equal <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_enddate,
    comparison_type = "less_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$inter_activity_date <- trip_date_activity_data_detail$trip_startdate_greater_equal & trip_date_activity_data_detail$trip_enddate_less_equal
  # Calculation if the date is outside the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_less <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_startdate,
    comparison_type = "less",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_greater <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_enddate,
    comparison_type = "greater",
    output = "report"
  )$logical
  trip_date_activity_data_detail$exter_activity_date <- trip_date_activity_data_detail$trip_startdate_less | trip_date_activity_data_detail$trip_enddate_greater
  # Calculates the number of occurrences of each activity date
  trip_date_activity_data_detail <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate, activity_date, inter_activity_date, exter_activity_date) %>%
    dplyr::summarise(count_freq = length(activity_date), .groups = "drop")
  # Calculation if an inconsistency among the different tests on the date has been found
  trip_date_activity_data_detail$logical <- trip_date_activity_data_detail$inter_activity_date & !trip_date_activity_data_detail$exter_activity_date & trip_date_activity_data_detail$count_freq == 1
  trip_date_activity_data_detail <- trip_date_activity_data_detail %>%
    data.frame()
  # Calculation if the number of days is consistent and if there are inconsistencies in the dates for the trips
  dataframe1 <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(nb_day = length(activity_date), logical_tmp = all(logical), .groups = "drop")
  # Calculation if an inconsistency among the different tests on the trip has been found
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(logical_nb_day = (1 + trip_enddate - trip_startdate) == nb_day) %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(logical = all(logical_nb_day, logical_tmp), .groups = "drop")
  # Management of missing trip start and end date
  dataframe1[is.na(dataframe1$trip_startdate) | is.na(dataframe1$trip_enddate), "logical"] <- FALSE
  dataframe1 <- subset(dataframe1, select = -c(trip_startdate, trip_enddate, logical_tmp, nb_day)) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with missing or surplus days")))
  }
  if (output == "report") {
    return(list(dataframe1, trip_date_activity_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_harbour_inspector
#' @title Gives the inconsistencies between the harbour of landing of the previous trip and the harbour of departure of the current trip
#' @description The purpose of the check_harbour_inspector function is to provide a table of data that contains an inconsistency with the harbour of landing of the previous trip and the harbour of departure of the current trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_harbour_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_id_departure}}
#'  \item{\code{  harbour_label_departure}}
#'  \item{\code{  trip_previous_id}}
#'  \item{\code{  harbour_id_landing_trip_previous}}
#'  \item{\code{  harbour_label_landing_trip_previous}}
#' }
#' @doctest
#' #Trip 1 and 2 are ok,
#' #Trip 3 has different harbour,
#' #Trip 4 has no harbour departure,
#' #Trip 5 has no harbour landing for trip previous
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          harbour_id_departure = c("1", "2", "3", NA, "1"),
#'                          harbour_label_departure = c("Harbour_1", "Harbour_2", "Harbour_3", NA,
#'                                                      "Harbour_1"),
#'                          trip_previous_id = c(NA, "1", "2", NA, "6"),
#'                          harbour_id_landing_trip_previous = c(NA, "2", "2", NA, NA),
#'                          harbour_label_landing_trip_previous = c(NA, "Harbour_2", "Harbour_2", NA,
#'                                                                  NA))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE), harbour_label_landing_trip_previous = c(NA, "Harbour_2", "Harbour_2", NA, NA), harbour_label_departure = c("Harbour_1", "Harbour_2", "Harbour_3", NA, "Harbour_1")), row.names = c(NA, 5L), class = "data.frame"))
#' check_harbour_inspector(dataframe1, output = "report")
#' @export
check_harbour_inspector <- function(dataframe1,
                                    output) {
  # 0 - Global variables assignement ----
  harbour_label_departure <- NULL
  harbour_label_landing_trip_previous <- NULL
  harbour_id_departure <- NULL
  harbour_id_landing_trip_previous <- NULL
  trip_previous_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous"),
    column_type = c("character", "character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous"),
      column_type = c("character", "character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare landing total weight of the trip with vessel capacity
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$harbour_id_landing_trip_previous,
    second_vector = dataframe1$harbour_id_departure,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of missing harbour landing in trip previous
  dataframe1[is.na(dataframe1$harbour_id_landing_trip_previous), "logical"] <- FALSE
  # Management of not trip previous
  dataframe1[is.na(dataframe1$trip_previous_id), "logical"] <- TRUE
  # Management of missing harbour departure
  dataframe1[is.na(dataframe1$harbour_id_departure), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, harbour_label_landing_trip_previous, harbour_label_departure, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(harbour_id_departure, harbour_id_landing_trip_previous, trip_previous_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with departure port different from the landing harbour of a previous trip")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_raising_factor_inspector
#' @title Gives the inconsistencies between RF1 and threshold values
#' @description The purpose of the check_raising_factor_inspector function is to provide a table of data that contains an inconsistency with the RF1 and the valid threshold (Default : 0.9 ; 1.1)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param country_species {\link[base]{character}} expected. Default values: list("1" = c("TUN", "ALB", "YFT", "BET", "SKJ"), "4" = c("LOT", "TUN", "ALB", "YFT", "BET", "SKJ", "LTA", "FRI", "BLF", "RAV*", "KAW", "FRZ", "BLT")). list of the inventory of species (FAO code) used to calculate catch weight in RF1 by country (country code).
#' @param species_fate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to calculate catch weight in RF1.
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("23", "25", "27", "29"). Vector of inventory of vessel activity NOT used to calculate catch weight in RF1.
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.9 and 1.1. Vector containing the lower and upper acceptable threshold for RF1.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_end_full_trip_id}}
#'  \item{\code{  vessel_id}}
#'  \item{\code{  country_fleetcountry}}
#' }
#' @doctest
#' #Trip 1, 3, 4 and 7 are ok,
#' #Trip 2 has an RF1 outside the thresholds,
#' #Trip 5 has no landing weight,
#' #Trip 6 has no catch weight
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5", "6", "7"))
#' dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'                          catch_weight = c(10, 2, 15, 8, 5, 25, 4, 5, 9),
#'                          speciesfate_code = c("6", "6", "1", "6", "6", "6", "6", "6", "6"),
#'                          species_fao_code = c("TUN", "YFT", "TUN", "TUN", "TUN", "SKJ", "SKJ",
#'                                               "LOT", "BET"),
#'                          vesselactivity_code = c("1", "1", "1", "1", "1", "1", "23", "1", "1"),
#'                          trip_id = c("1", "1", "1", "2", "2", "3", "3", "3", "5"))
#' dataframe3 <- data.frame(landing_id = c("1" , "2", "3", "4", "5"),
#'                          landing_weight = c(9, 2, 8, 25, 12),
#'                          species_fao_code = c("YFT", "TUN", "TUN", "TUN", "BET"),
#'                          trip_id = c("1", "1", "2", "4", "6"))
#' dataframe4 <- data.frame(trip_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          trip_end_full_trip_id = c("1", "2", "4", "4", "5", "6", "7"),
#'                          vessel_id = c("1", "1", "1", "1", "1", "1", "1"),
#'                          country_fleetcountry = c("1", "1", "1", "1", "1", "1", "1"))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE), rf1 = c(0.916666666666667, 0.615384615384615, 1, 1, NA, NA, NA)), row.names = c(NA, 7L), class = "data.frame"))
#' check_raising_factor_inspector(dataframe1, dataframe2, dataframe3, dataframe4, output = "report")
#' @export
check_raising_factor_inspector <- function(dataframe1,
                                           dataframe2,
                                           dataframe3,
                                           dataframe4,
                                           output,
                                           country_species = list("1" = c("TUN", "ALB", "YFT", "BET", "SKJ"), "4" = c("LOT", "TUN", "ALB", "YFT", "BET", "SKJ", "LTA", "FRI", "BLF", "RAV*", "KAW", "FRZ", "BLT")),
                                           species_fate = "6",
                                           vessel_activity = c("23", "25", "27", "29"),
                                           threshold = c(0.9, 1.1)) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  catch_weight <- NULL
  sum_catch_weight <- NULL
  rf1 <- NULL
  full_trip_sum_landing_weight <- NULL
  full_trip_sum_catch_weight <- NULL
  lower_threshold <- NULL
  upper_threshold <- NULL
  speciesfate_code <- NULL
  vesselactivity_code <- NULL
  country_fleetcountry <- NULL
  landing_weight <- NULL
  sum_landing_weight <- NULL
  trip_end_full_trip_id <- NULL
  vessel_id <- NULL
  logical_full_trip <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id"),
    column_type = c("character", "numeric", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id"),
      column_type = c("character", "numeric", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("landing_id", "landing_weight", "species_fao_code", "trip_id"),
    column_type = c("character", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("landing_id", "landing_weight", "species_fao_code", "trip_id"),
      column_type = c("character", "numeric", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("landing_id", "landing_weight", "species_fao_code", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe4 <- dataframe4[, c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  # Checks the type of country_species
  if (!codama::r_type_checking(
    r_object = country_species,
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = country_species,
      type = "list",
      output = "error"
    ))
  }
  # Checks the type of species_fate
  if (!codama::r_type_checking(
    r_object = species_fate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_fate,
      type = "character",
      output = "error"
    ))
  }
  # Checks the type of vessel_activity
  if (!codama::r_type_checking(
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "error"
    ))
  }
  # Checks the type of threshold
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 2L,
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Add country_fleetcountry for catch
  dataframe2 <- dplyr::left_join(dataframe2, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = dplyr::join_by(trip_id))
  # Catch filtration for RF1
  ## Selection species when the list is available for the country and selection species
  condition <- as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe2_select_species <- purrr::map(condition, ~ dataframe2 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe2_select_species <- dplyr::bind_rows(dataframe2_select_species)
  ## Selection all species when the list is not available for the country
  dataframe2 <- dplyr::bind_rows(dataframe2_select_species, dataframe2 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  ## Selection species fate
  dataframe2 <- dataframe2 %>%
    dplyr::filter((speciesfate_code %in% species_fate))
  ## Selection vessel activity
  dataframe2 <- dataframe2 %>%
    dplyr::filter(!(vesselactivity_code %in% vessel_activity))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- dplyr::left_join(dataframe4, dataframe2, by = dplyr::join_by(trip_id))
  # Add country_fleetcountry for landing
  dataframe3 <- dplyr::left_join(dataframe3, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = dplyr::join_by(trip_id))
  # Landing filtration for RF1
  ## Selection species when the list is available for the country and selection species fate
  condition <- as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe3_select_species <- purrr::map(condition, ~ dataframe3 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe3_select_species <- dplyr::bind_rows(dataframe3_select_species)
  ## Selection all species when the list is not available for the country
  dataframe3 <- dplyr::bind_rows(dataframe3_select_species, dataframe3 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_landing_weight = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- dplyr::left_join(dataframe4, dataframe3, by = dplyr::join_by(trip_id))
  # Add of a logic that indicates whether the full trip is finished or not
  dataframe4$logical_full_trip <- !is.na(dataframe4$trip_end_full_trip_id)
  # For unfinished full trips (no end-of-full-trip id) indicates the vessel id for the end-of-full-trip id (for each ship, allows you to group together all the trips of the non-finished full trip)
  dataframe4[is.na(dataframe4$trip_end_full_trip_id), "trip_end_full_trip_id"] <- paste0("vessel_id_", dataframe4[is.na(dataframe4$trip_end_full_trip_id), "vessel_id", drop = TRUE])
  # RF1 calculation
  full_trip_selected_rf1 <- dataframe4 %>%
    dplyr::group_by(trip_end_full_trip_id) %>%
    dplyr::summarise(full_trip_sum_landing_weight = ifelse(all(is.na(sum_landing_weight)), sum_landing_weight[NA_integer_], sum(sum_landing_weight, na.rm = TRUE)), full_trip_sum_catch_weight = ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(rf1 = full_trip_sum_landing_weight / full_trip_sum_catch_weight)
  if (nrow(dataframe4) > 0) {
    dataframe4$lower_threshold <- threshold[1]
    dataframe4$upper_threshold <- threshold[2]
  } else {
    dataframe4$lower_threshold <- numeric()
    dataframe4$upper_threshold <- numeric()
  }
  # Selection of user-supplied trips
  dataframe4 <- dplyr::left_join(data.frame(trip_id = dataframe1$trip_id), unique(dataframe4), by = dplyr::join_by(trip_id))
  # Merge data
  dataframe4 <- dplyr::left_join(dataframe4, full_trip_selected_rf1, by = dplyr::join_by(trip_end_full_trip_id))
  # Compare RF1 to valid threshold
  comparison_less <- codama::vector_comparison(
    first_vector = dataframe4$rf1,
    second_vector = dataframe4$upper_threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_greater <- codama::vector_comparison(
    first_vector = dataframe4$rf1,
    second_vector = dataframe4$lower_threshold,
    comparison_type = "greater_equal",
    output = "report"
  )
  dataframe4$logical <- comparison_less$logical & comparison_greater$logical
  # Corrects missing RF1s when nothing has been landed and there is no capture
  dataframe4[(is.na(dataframe4$full_trip_sum_landing_weight) | dataframe4$full_trip_sum_landing_weight == 0) & is.na(dataframe4$full_trip_sum_catch_weight), "logical"] <- TRUE
  dataframe4 <- dplyr::relocate(.data = dataframe4, rf1, .after = logical)
  dataframe4 <- subset(dataframe4, select = -c(trip_end_full_trip_id, logical_full_trip, sum_catch_weight, sum_landing_weight, full_trip_sum_landing_weight, full_trip_sum_catch_weight, lower_threshold, upper_threshold, vessel_id, country_fleetcountry))
  if ((sum(dataframe4$logical, na.rm = TRUE) + sum(!dataframe4$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe4$logical))) {
    all <- c(select, dataframe4$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe4$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe4$logical)), "):", paste0(dataframe4$trip_id[is.na(dataframe4$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe4$logical), " trips with RF1 outside defined thresholds or missing")))
  }
  if (output == "report") {
    return(dataframe4)
  }
  if (output == "logical") {
    if (sum(!dataframe4$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' @name check_fishing_context_inspector
#' @title Gives the inconsistencies between the school type and the association
#' @description The purpose of the check_fishing_context_inspector function is to provide a table of data that contains an inconsistency with school type and the association
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_context_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_context_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param school_type_object {\link[base]{character}} expected, default : c("1"). Vector of inventory of code of the object school.
#' @param school_type_free {\link[base]{character}} expected, default : c("2"). Vector of inventory of code of the free school.
#' @param school_type_unknown {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the unknown school.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' }
#' @doctest
#' #Activity 1, 2, 5, 6 and 7 are ok,
#' #Activity 3 has school type object,
#' #Activity 4 has no school type object
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          schooltype_code = c("2", "1", "2", "1", NA, "0", "2"))
#' dataframe2 <- data.frame(observedsystem_id = c("1", "2", "3", "4", "5"),
#'                          activity_id = c("1", "2", "2", "3", "4"),
#'                          schooltype_code = c("2", "2", "1", "1", "2"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE), schooltype_code = c("2", "1", "2", "1", NA, "0", "2"), association_object_count = c(0, 1, 1, 0, 0, 0, 0)), row.names = c(NA, 7L), class = "data.frame"))
#' check_fishing_context_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_fishing_context_inspector <- function(dataframe1,
                                            dataframe2,
                                            output,
                                            school_type_object = c("1"),
                                            school_type_free = c("2"),
                                            school_type_unknown = c("0")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  schooltype_code <- NULL
  association_object_count <- NULL
  threshold <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "schooltype_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "schooltype_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "schooltype_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("observedsystem_id", "activity_id", "schooltype_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("observedsystem_id", "activity_id", "schooltype_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("observedsystem_id", "activity_id", "schooltype_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type_object,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_object,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type_free,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_free,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_unknown,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Filters out object-school associations
  dataframe2 <- dataframe2 %>% dplyr::filter(schooltype_code %in% school_type_object)
  # Calculates the number of object-type associations
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(association_object_count = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- 0
  } else {
    dataframe1$threshold <- numeric()
  }
  dataframe1$association_object_count[is.na(dataframe1$association_object_count)] <- 0
  # Indicates whether or not an object-type association exists
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$association_object_count,
    second_vector = dataframe1$threshold,
    comparison_type = "greater",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Case of free school : must not have any object-type association (inverse of the result obtained)
  dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code %in% school_type_free] <- !dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code %in% school_type_free]
  # Unknown school and NA: no constraint
  dataframe1$logical[is.na(dataframe1$schooltype_code) | dataframe1$schooltype_code %in% school_type_unknown] <- TRUE
  dataframe1 <- dplyr::relocate(.data = dataframe1, schooltype_code, association_object_count, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with school types that do not correspond to the observed associations")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_operation_inspector
#' @title Gives the inconsistencies between fishing success status, vessel activity, type of school or weight caught
#' @description The purpose of the check_operation_inspector function is to provide a table of data that contains an inconsistency with succes status and vessel activity, the type of school or the weight caught
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_operation_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector of inventory of codes for activities that must have a success status
#' @param school_type {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the school type must not have specific success status (defined success_status_school_type)
#' @param success_status_school_type {\link[base]{character}} expected, default : c("0", "1"). Vector of inventory of code of the success status must not have specific school type (defined school_type)
#' @param success_status_weight {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the success status must not have weigth
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#'  \item{\code{  successstatus_code}}
#'  \item{\code{  activity_weight}}
#'  \item{\code{  vesselactivity_code}}
#' }
#' @doctest
#' #Activity 1, 2, 4 and 8 are ok,
#' #Activity 3 has no weight,
#' #Activity 5 has success status,
#' #Activity 6 has no school type,
#' #Activity 7 has no success status,
#' #Activity 9 has success status in success_status_school_type,
#' #Activity 10 has weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
#'                          schooltype_code = c("2", "1", "2", NA, NA, NA, "1", "0", "0", "0", "1"),
#'                          successstatus_code = c("1", "0", "2", NA, "2", "2", NA, "2", "1", "2",
#'                                                 "0"),
#'                          activity_weight = c(10, 0, 0, NA, NA, 6, 8, 2, 8, NA, 4),
#'                          vesselactivity_code = c("6", "6", "6", "1", "1", "6", "6", "6", "6", "6",
#'                                                  "6"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), vesselactivity_code = c("6", "6", "6", "1", "1", "6", "6", "6", "6", "6", "6"), successstatus_code = c("1", "0", "2", NA, "2", "2", NA, "2", "1", "2", "0"), schooltype_code = c("2", "1", "2", NA, NA, NA, "1", "0", "0", "0", "1"), activity_weight = c(10, 0, 0, NA, NA, 6, 8, 2, 8, NA, 4)), row.names = c(NA, 11L), class = "data.frame"))
#' check_operation_inspector(dataframe1, output = "report")
#' @export
check_operation_inspector <- function(dataframe1,
                                      output,
                                      vessel_activity = c("6"),
                                      school_type = c("0"),
                                      success_status_school_type = c("0", "1"),
                                      success_status_weight = c("0")) {
  # 0 - Global variables assignement ----
  vesselactivity_code <- NULL
  successstatus_code <- NULL
  schooltype_code <- NULL
  activity_weight <- NULL
  threshold <- NULL
  logical_successstatus_vesselactivity <- NULL
  logical_successstatus_schooltype <- NULL
  logical_successstatus_weight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code"),
    column_type = c("character", "character", "character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code"),
      column_type = c("character", "character", "character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = success_status_school_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = success_status_school_type,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = success_status_weight,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = success_status_weight,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Indicates whether the vessel activity requires a success status
  comparison_successstatus_vesselactivity <- codama::vector_comparison(
    first_vector = dataframe1$vesselactivity_code,
    second_vector = vessel_activity,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_successstatus_vesselactivity <- comparison_successstatus_vesselactivity$logical
  # Case of success status NA: must not have activity 6 (inverse of the result obtained)
  dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)] <- !dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)]
  # Indicates indeterminate school must not have positive or negative success status
  logical_successstatus_schooltype_indeterminate <- codama::vector_comparison(
    first_vector = dataframe1$schooltype_code,
    second_vector = school_type,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_successstatus_schooltype_indeterminate <- !logical_successstatus_schooltype_indeterminate$logical
  # Case of success status indeterminate or NA: no constraints
  dataframe1$logical_successstatus_schooltype_indeterminate[!(dataframe1$successstatus_code %in% success_status_school_type)] <- TRUE
  # Indicates whether the success status requires a school type
  dataframe1$logical_successstatus_schooltype <- !is.na(dataframe1$schooltype_code)
  # Case of success status NA: no constraints
  dataframe1$logical_successstatus_schooltype[is.na(dataframe1$successstatus_code)] <- TRUE
  # Indicates whether captured weight is greater than 0
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- 0
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_successstatus_weight <- codama::vector_comparison(
    first_vector = dataframe1$activity_weight,
    second_vector = dataframe1$threshold,
    comparison_type = "greater",
    output = "report"
  )
  dataframe1$logical_successstatus_weight <- comparison_successstatus_weight$logical
  # Case of success status null : must not have weight (inverse of the result obtained)
  dataframe1$logical_successstatus_weight[dataframe1$successstatus_code %in% success_status_weight] <- !dataframe1$logical_successstatus_weight[dataframe1$successstatus_code %in% success_status_weight]
  # NA success status: no constraints
  dataframe1$logical_successstatus_weight[is.na(dataframe1$successstatus_code)] <- TRUE
  # Combines test results
  dataframe1$logical <- dataframe1$logical_successstatus_vesselactivity & dataframe1$logical_successstatus_schooltype_indeterminate & dataframe1$logical_successstatus_schooltype & dataframe1$logical_successstatus_weight
  dataframe1 <- dplyr::relocate(.data = dataframe1, vesselactivity_code, successstatus_code, schooltype_code, activity_weight, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold, logical_successstatus_vesselactivity, logical_successstatus_schooltype_indeterminate, logical_successstatus_schooltype, logical_successstatus_weight))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with a succes status that doesn't match either the vessel activity, the type of school or the weight caught.")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_position_inspector
#' @title Gives the inconsistencies between the ocean declared for the trip and the position of the activity
#' @description The purpose of the check_position_inspector function is to provide a table of data that contains an inconsistency with ocean declaration and activity position
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_position_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_position_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Layer to containing the seas shapefile (example cf : cf : Flanders Marine Institute. IHO Sea Areas, version 1. Available online at https://www.marineregions.org/)
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param harbour_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position harbour
#' @param buffer_harbour {\link[base]{numeric}} expected. Default values: 11100. Buffer to be used for harbour, in meter
#' @param buffer_sea {\link[base]{numeric}} expected. Default values: 925. Buffer to be used for seas, in meter
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  ocean_label}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_position_departure}}
#'  \item{\code{  harbour_position_landing}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  ID}}
#'  \item{\code{  geometry}}
#' }
#' @doctest
#' #Activity 1, 3 and 4 are ok,
#' #Activity 2 has different ocean,
#' #Activity 5 is outside ocean and harbour,
#' #Activity 6 has no position
#' #Activity 7 has no ocean label
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2",
#'                                          "ocean_1", NA),
#'                          activity_position = c("POINT (0 0)", "POINT (0 0)", "POINT (-1 -1)",
#'                                                "POINT (1 1)", "POINT (3 3)", NA, "POINT (1 1)"),
#'                          trip_id = c("1", "2", "2", "2", "2", "2", "2"))
#' dataframe2 <- data.frame(trip_id = c("1", "2"),
#'                          harbour_position_departure = c("POINT (20 20)", "POINT (-1.05 -1)"),
#'                          harbour_position_landing = c("POINT (20 20)", "POINT (20 20)"))
#' dataframe3 <- sf::st_sf(data.frame(ID = c("ocean_1", "ocean_2"),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          sf::st_polygon(list(rbind(c(0,1), c(3,1),
#'                                                                                    c(3,2), c(0,2),
#'                                                                                    c(0,1)))),
#'                                                          crs = 4326)))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), type = c("Sea", "Sea", "Harbour", "Sea", "Excluding shapes oceans", "No position", "Sea"), ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2", "ocean_1", NA), ocean_calculate = c("ocean_1", "ocean_1", NA, "ocean_1 ocean_2", NA, NA, "ocean_1 ocean_2")), row.names = c(NA, 7L), class = "data.frame"), structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), activity_position = c("POINT (0 0)", "POINT (0 0)", "POINT (-1 -1)", "POINT (1 1)", "POINT (3 3)", NA, "POINT (1 1)"), logical_harbour = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), logical_ocean = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), type = c("Sea", "Sea", "Harbour", "Sea", "Excluding shapes oceans", "No position", "Sea"), ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2", "ocean_1", NA), ocean_calculate = c("ocean_1", "ocean_1", NA, "ocean_1 ocean_2", NA, NA, "ocean_1 ocean_2"), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, 7L), class = "data.frame")))
#' check_position_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_position_inspector <- function(dataframe1,
                                     dataframe2,
                                     dataframe3,
                                     output,
                                     activity_crs = 4326,
                                     harbour_crs = 4326,
                                     buffer_harbour = 11100,
                                     buffer_sea = 925) {
  # 0 - Global variables assignement ----
  . <- NULL
  type <- NULL
  ocean_label <- NULL
  activity_position <- NULL
  harbour_position_departure <- NULL
  harbour_buffer_departure <- NULL
  harbour_position_landing <- NULL
  harbour_buffer_landing <- NULL
  logical_ocean <- NULL
  logical_harbour <- NULL
  logical_harbourdeparture <- NULL
  logical_harbourlanding <- NULL
  ocean_calculate <- NULL
  trip_id <- NULL
  activity_id <- NULL
  X <- NULL
  Y <- NULL
  logical_bounding <- NULL
  intersect_indice <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "ocean_label", "activity_position", "trip_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "ocean_label", "activity_position", "trip_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "ocean_label", "activity_position", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "harbour_position_departure", "harbour_position_landing"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "harbour_position_departure", "harbour_position_landing"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "harbour_position_departure", "harbour_position_landing")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("ID", "geometry"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("ID", "geometry"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("ID", "geometry")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = harbour_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = harbour_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = buffer_harbour,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = buffer_harbour,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = buffer_sea,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = buffer_sea,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(select)
  # 2 - Data design ----
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Indicates whether in land harbour
  if (nrow(dataframe1) > 0) {
    dataframe1$logical_harbour <- FALSE
  } else {
    dataframe1$logical_harbour <- logical()
  }
  # Formats spatial data activity
  data_geo_activity <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(., wkt = "activity_position", crs = activity_crs) %>%
    sf::st_transform(activity_position, crs = 4326)
  # Retrieves point coordinates
  if (nrow(data_geo_activity) > 0) {
    data_geo_activity <- data_geo_activity %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
  } else {
    data_geo_activity$X <- numeric()
    data_geo_activity$Y <- numeric()
  }
  # Checks whether the point is within the bounds of CRS 4326
  data_geo_activity <- data_geo_activity %>%
    dplyr::mutate(logical_bounding = (X >= -180 & X <= 180 & Y >= -90 & Y <= 90)) %>%
    dplyr::filter(logical_bounding)
  # If harbour departure position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_departure))) {
    # Formats spatial data harbourdeparture, add buffer in meter
    data_geo_harbourdeparture <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_departure)) %>%
      dplyr::select(harbour_position_departure) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_departure = harbour_position_departure) %>%
      sf::st_as_sf(., wkt = "harbour_buffer_departure", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_departure, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourdeparture
    data_geo_activity_harbourdeparture <- data_geo_activity %>% dplyr::filter(!is.na(harbour_position_departure))
    intersect_harbourdeparture <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) lengths(sf::st_intersects(data_geo_activity_harbourdeparture[data_geo_activity_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x], 1], data_geo_harbourdeparture[x, 1])))
    intersect_harbourdeparture_index <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) which(data_geo_activity_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x]))
    data_geo_activity_harbourdeparture$nb_port_intersect <- 0
    data_geo_activity_harbourdeparture$nb_port_intersect[unlist(intersect_harbourdeparture_index)] <- unlist(intersect_harbourdeparture)
    data_geo_activity_harbourdeparture$logical_harbourdeparture <- data_geo_activity_harbourdeparture$nb_port_intersect != 0
    # Logical harbourdeparture
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(data_geo_activity_harbourdeparture)[, c("activity_id", "logical_harbourdeparture")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourdeparture[is.na(dataframe1$logical_harbourdeparture)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourdeparture <- FALSE
    } else {
      dataframe1$logical_harbourdeparture <- logical()
    }
  }
  # If harbour landing position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_landing))) {
    # Formats spatial data harbourlanding, add buffer in meter
    data_geo_harbourlanding <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_landing)) %>%
      dplyr::select(harbour_position_landing) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_landing = harbour_position_landing) %>%
      sf::st_as_sf(., wkt = "harbour_buffer_landing", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_landing, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourlanding
    data_geo_activity_harbourlanding <- data_geo_activity %>% dplyr::filter(!is.na(harbour_position_landing))
    intersect_harbourlanding <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) lengths(sf::st_intersects(data_geo_activity_harbourlanding[data_geo_activity_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x], 1], data_geo_harbourlanding[x, 1])))
    intersect_harbourlanding_index <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) which(data_geo_activity_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x]))
    data_geo_activity_harbourlanding$nb_port_intersect <- 0
    data_geo_activity_harbourlanding$nb_port_intersect[unlist(intersect_harbourlanding_index)] <- unlist(intersect_harbourlanding)
    data_geo_activity_harbourlanding$logical_harbourlanding <- data_geo_activity_harbourlanding$nb_port_intersect != 0
    # Logical in harbourlanding
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(data_geo_activity_harbourlanding)[, c("activity_id", "logical_harbourlanding")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourlanding[is.na(dataframe1$logical_harbourlanding)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourlanding <- FALSE
    } else {
      dataframe1$logical_harbourlanding <- logical()
    }
  }
  # Logical in harbour
  dataframe1$logical_harbour <- dataframe1$logical_harbour | dataframe1$logical_harbourdeparture | dataframe1$logical_harbourlanding
  # Buffer sea in degrees
  shape_sea_buffer <- dataframe3 %>%
    terra::vect() %>%
    terra::buffer(width = buffer_sea) %>%
    sf::st_as_sf()
  # Calculates the intersection between activity and sea and logical in sea, indicates whether the ocean is the same
  intersect <- data_geo_activity %>%
    dplyr::mutate(intersect_indice = sf::st_intersects(data_geo_activity, shape_sea_buffer)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(logical_ocean = ocean_label %in% shape_sea_buffer$ID[unlist(intersect_indice)],
                  ocean_calculate = ifelse(length(shape_sea_buffer$ID[unlist(intersect_indice)]) > 0, paste0(shape_sea_buffer$ID[unlist(intersect_indice)], collapse = " "), NA)) %>%
    dplyr::ungroup()
  dataframe1 <- dplyr::left_join(dataframe1, data.frame(intersect)[, c("activity_id", "logical_ocean", "ocean_calculate")], by = dplyr::join_by(activity_id))
  dataframe1$logical_ocean[is.na(dataframe1$logical_ocean)] <- FALSE
  # Case of harbour in sea : not in harbour
  dataframe1$logical_harbour[!is.na(dataframe1$ocean_calculate)] <- FALSE
  dataframe1$logical <- dataframe1$logical_ocean | dataframe1$logical_harbour
  # Gives the type of location
  if (nrow(dataframe1) > 0) {
    dataframe1$type <- "Sea"
  } else {
    dataframe1$type <- character()
  }
  dataframe1$type[is.na(dataframe1$ocean_calculate)] <- "Excluding shapes oceans"
  dataframe1$type[is.na(dataframe1$activity_position)] <- "No position"
  dataframe1$type[dataframe1$logical_harbour] <- "Harbour"
  dataframe1 <- dplyr::relocate(.data = dataframe1, type, ocean_label, ocean_calculate, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(trip_id, harbour_position_departure, harbour_position_landing, logical_harbourdeparture, logical_harbourlanding))
  activity_sea_land_data_detail <- dataframe1 %>%
    dplyr::mutate(activity_crs = activity_crs)
  dataframe1 <- subset(dataframe1, select = -c(activity_position, logical_ocean, logical_harbour))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with position is inconsistent, outside the ocean declared for the trip")))
  }
  if (output == "report") {
    return(list(dataframe1, activity_sea_land_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_weight_inspector
#' @title Gives the inconsistencies between the sum of the weight indicated for catches and the one indicated for the activity
#' @description The purpose of the check_weight_inspector function is to provide a table of data that contains an inconsistency between the sum of the weight indicated for the catch and the one indicated for the activity
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weight_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weight_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_weight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Activity 1, 2 and 4 are ok,
#' #Activity 3 has different weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4"),
#'                          activity_weight = c(10, 15.01, 6, NA))
#' dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5"),
#'                          catch_weight = c(10, 10, 5, 3, 2),
#'                          activity_id = c("1", "2", "2", "3", "3"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE, FALSE, TRUE), activity_weight = c(10, 15.01, 6, NA), sum_catch_weight = c(10, 15, 5, NA)), row.names = c(NA, 4L), class = "data.frame"))
#' check_weight_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_weight_inspector <- function(dataframe1,
                                   dataframe2,
                                   output,
                                   epsilon = 0.01) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  catch_weight <- NULL
  activity_weight <- NULL
  sum_catch_weight <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_weight"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_weight"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_weight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "activity_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "activity_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  # Checks the type of epsilon
  if (!codama::r_type_checking(
    r_object = epsilon,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = epsilon,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the sum of the weight per activity (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge and calcul difference
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  dataframe1$difference <- ifelse(is.na(dataframe1$activity_weight), 0, dataframe1$activity_weight) - ifelse(is.na(dataframe1$sum_catch_weight), 0, dataframe1$sum_catch_weight)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare weight of the activity or the sum of the catch
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of the NA value for the weight activity and catch
  dataframe1[is.na(dataframe1$activity_weight) & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  # Management of the 0 value for the weight activity
  dataframe1[!is.na(dataframe1$activity_weight) & dataframe1$activity_weight == 0 & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, activity_weight, sum_catch_weight, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(difference, epsilon))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with a weight different from the sum of the weight of each catch")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_length_class_inspector
#' @title Gives the inconsistencies between size class of the samples depending on the species and measurement type and the valid threshold
#' @description The purpose of the check_length_class_inspector function is to provide a table of data that contains an inconsistency between the size class of the samples depending on the species and measurement type and the valid threshold (Default : 80)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_length_class_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "BET", "ALB"). Vector of the species inventory controlled.
#' @param size_measure_type {\link[base]{character}} expected. Default values: "FL". Vector of the size measure type controlled.
#' @param threshold {\link[base]{numeric}} expected. Default values: 80. Maximum size threshold measured
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#' }
#' @doctest
#' #Activity 1, 2 and 3 are ok,
#' #Activity 4 has size class is greater than threshold,
#' #Activity 5 has size class is missing
#' dataframe1 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"),
#'                          species_fao_code = c("YFT", "YFT", "LTA", "YFT", "YFT"),
#'                          sizemeasuretype_code = c("FL", "PD1", "FL", "FL", "FL"),
#'                          samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA))
#' @expect equal(., structure(list(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' check_length_class_inspector(dataframe1, output = "report")
#' @export
check_length_class_inspector <- function(dataframe1,
                                         output,
                                         species = c("YFT", "BET", "ALB"),
                                         size_measure_type = "FL",
                                         threshold = 80) {
  # 0 - Global variables assignement ----
  species_fao_code <- NULL
  sizemeasuretype_code <- NULL
  samplespeciesmeasure_sizeclass <- NULL
  logical_sizeclass <- NULL
  logical_sizemeasuretype <- NULL
  logical_species <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
    column_type = c("character", "character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
      column_type = c("character", "character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$samplespeciesmeasure_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold
  } else {
    dataframe1$threshold <- numeric()
  }
  # Compare size class of the samples
  comparison_sizeclass <- codama::vector_comparison(
    first_vector = dataframe1$samplespeciesmeasure_sizeclass,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical_sizeclass <- comparison_sizeclass$logical
  # Compare specie of the samples
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_species <- comparison_species$logical
  # Compare size measure type of the samples
  comparison_sizemeasuretype <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_sizemeasuretype <- comparison_sizemeasuretype$logical
  dataframe1$logical <- dataframe1$logical_sizeclass | !dataframe1$logical_sizemeasuretype | !dataframe1$logical_species
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, species_fao_code, sizemeasuretype_code, samplespeciesmeasure_sizeclass, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_sizeclass, logical_sizemeasuretype, logical_species, threshold, species_fao_code, sizemeasuretype_code))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespeciesmeasure_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespeciesmeasure_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with measurements ", paste0(size_measure_type, collapse = ", "), ", for species ", paste0(species, collapse = ", "), ", greater than ", threshold)))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_measure_inspector
#' @title Gives the inconsistencies between the total number of individuals measured per sample and the sum of individuals per sample, species and size class
#' @description The purpose of the check_measure_inspector function is to provide a table of data that contains an inconsistency between the total number of individuals measured per sample and the sum of individuals per sample, species and size class
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_measure_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_measure_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespecies_measuredcount}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 has different count,
#' #Sample 4 has sample species measure count is missing in dataframe2,
#' #Sample 5 has sample species measured count is missing in dataframe1
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6"),
#'                          samplespecies_measuredcount = c(4, 6, 15, 6, 7, NA),
#'                          sample_id = c("1", "1", "2", "3", "4", "5"))
#' dataframe2 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5", "6"),
#'                          samplespeciesmeasure_count = c(10, 10, 5, 3, 2, 8),
#'                          samplespecies_id = c("1", "3", "3", "4", "4", "6"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE), sum_measuredcount = c(10, 15, 6, 7, NA), sum_count = c(10, 15, 5, NA, 8)), row.names = c(NA, -5L), class = "data.frame"))
#' check_measure_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_measure_inspector <- function(dataframe1,
                                    dataframe2,
                                    output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  samplespecies_id <- NULL
  samplespecies_measuredcount <- NULL
  samplespeciesmeasure_count <- NULL
  sum_measuredcount <- NULL
  sum_count <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespecies_measuredcount", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespecies_measuredcount", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "samplespecies_measuredcount", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculates the total sum of individuals by sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(samplespecies_id) %>%
    dplyr::summarise(sum_count = ifelse(all(is.na(samplespeciesmeasure_count)), samplespeciesmeasure_count[NA_integer_], sum(samplespeciesmeasure_count, na.rm = TRUE)))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(samplespecies_id))
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_measuredcount = ifelse(all(is.na(samplespecies_measuredcount)), samplespecies_measuredcount[NA_integer_], sum(samplespecies_measuredcount, na.rm = TRUE)),
                     sum_count = ifelse(all(is.na(sum_count)), sum_count[NA_integer_], sum(sum_count, na.rm = TRUE)))
  # Compare the two sums
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$sum_measuredcount,
    second_vector = dataframe1$sum_count,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, sum_measuredcount, sum_count, .after = logical) %>%
    data.frame()
  # Management of missing count measurements by sample and by species
  dataframe1[is.na(dataframe1$sum_measuredcount), "logical"] <- FALSE
  # Management of missing count measurements by sample and by species and by size class
  dataframe1[is.na(dataframe1$sum_count), "logical"] <- FALSE
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with number of individuals measured per species different from number measured per species and size class")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_temperature_inspector
#' @title Gives the inconsistencies between activity sea surface temperature and valid threshold
#' @description The purpose of the check_temperature_inspector function is to provide a table of data that contains an inconsistency between the sea surface temperature for the activity and valid threshold
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_temperature_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold {\link[base]{numeric}} expected. Default values: 15 and 32. Vector containing the lower and upper acceptable threshold for sea surface temperature.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_seasurfacetemperature}}
#' }
#' @doctest
#' #Activity 1 is ok,
#' #Activity 2 and 3 have temperatures outside the thresholds
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3"),
#'                          activity_seasurfacetemperature = c(20, 4, 35))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3"), logical = c(TRUE, FALSE, FALSE), activity_seasurfacetemperature = c(20, 4, 35)), row.names = c(NA, 3L), class = "data.frame"))
#' check_temperature_inspector(dataframe1, output = "report")
#' @export
check_temperature_inspector <- function(dataframe1,
                                        output,
                                        threshold = c(15, 32)) {
  # 0 - Global variables assignement ----
  activity_seasurfacetemperature <- NULL
  lower_threshold <- NULL
  upper_threshold <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_seasurfacetemperature"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_seasurfacetemperature"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_seasurfacetemperature")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 2L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare RF1 to valid threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$lower_threshold <- threshold[1]
    dataframe1$upper_threshold <- threshold[2]
  } else {
    dataframe1$lower_threshold <- numeric()
    dataframe1$upper_threshold <- numeric()
  }
  comparison_less <- codama::vector_comparison(
    first_vector = dataframe1$activity_seasurfacetemperature,
    second_vector = dataframe1$upper_threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_greater <- codama::vector_comparison(
    first_vector = dataframe1$activity_seasurfacetemperature,
    second_vector = dataframe1$lower_threshold,
    comparison_type = "greater_equal",
    output = "report"
  )
  dataframe1$logical <- comparison_less$logical & comparison_greater$logical
  # Management of the NA value for the sea surface temperature
  dataframe1[is.na(dataframe1$activity_seasurfacetemperature), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, activity_seasurfacetemperature, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(lower_threshold, upper_threshold))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with sea surface temperature outside defined thresholds")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_weighting_sample_inspector
#' @title Gives the inconsistencies between the sample weighting and catch weight
#' @description The purpose of the check_weighting_sample_inspector function is to provide a table of data that contains an inconsistency between the sample weighting and catch weight for activity
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_sample_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_sample_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_sample_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). list of the inventory of species (FAO code) used to compare to sample weighting.
#' @param species_fate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to compare to sample weighting.
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Activity 1, 3 and 5 are ok,
#' #Activity 2 has different weight,
#' #Activity 4 has weighted weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5"))
#' dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5", "6"),
#'                          catch_weight = c(4, 2, 5, 10, 3, 8),
#'                          speciesfate_code = c("6", "6", "6", "6", "6", "6"),
#'                          species_fao_code = c("YFT", "JOS", "ALB", "YFT", "YFT", "FRI"),
#'                          activity_id = c("1", "1", "1", "2", "2", "5"))
#' dataframe3 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5"),
#'                          sampleactivity_weightedweight = c(3, 6, 12.5, NA, 26),
#'                          activity_id = c("1", "1", "2", "3", "4"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, TRUE, FALSE, TRUE), weight = c(9, 13, NA, NA, 8), weightedweight = c(9, 12.5, 0, 26, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' check_weighting_sample_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_weighting_sample_inspector <- function(dataframe1,
                                             dataframe2,
                                             dataframe3,
                                             output,
                                             species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"),
                                             species_fate = "6",
                                             epsilon = 0.01) {
  # 0 - Global variables assignement ----
  sampleactivity_weightedweight <- NULL
  weightedweight <- NULL
  weight <- NULL
  activity_id <- NULL
  species_fao_code <- NULL
  speciesfate_code <- NULL
  catch_weight <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
    column_type = c("character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
      column_type = c("character", "numeric", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_fate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_fate,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = epsilon,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = epsilon,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::filter(species_fao_code %in% species & speciesfate_code %in% species_fate) %>%
    dplyr::summarise(weight = ifelse(all(is.na(catch_weight)), 0, sum(catch_weight, na.rm = TRUE)))
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weightedweight_calculation <- dataframe3 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)))
  # Merge
  dataframe2 <- dataframe2 %>%
    dplyr::select(activity_id) %>%
    dplyr::distinct()
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_weight_calculation, by = dplyr::join_by(activity_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_weightedweight_calculation, by = dplyr::join_by(activity_id))
  # Calcul difference
  dataframe1$difference <- ifelse(is.na(dataframe1$weight), 0, dataframe1$weight) - ifelse(is.na(dataframe1$weightedweight), 0, dataframe1$weightedweight)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of missing sample
  dataframe1[is.na(dataframe1$weightedweight) | (!is.na(dataframe1$weightedweight) & dataframe1$weightedweight == 0), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(epsilon, difference))
  dataframe1 <- dplyr::relocate(.data = dataframe1, weight, weightedweight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with weighted weight", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_time_route_inspector
#' @title Gives the inconsistencies between the fishing times or sea times indicated for the route and activities carried out
#' @description The purpose of the check_time_route_inspector function is to provide a table of data that contains an inconsistency between the fishing times or sea times indicated for the route and activities carried out
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_time_route_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_time_route_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_time_route_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold_sea_time {\link[base]{numeric}} expected. Default values: 24. Maximum valid time for sea time
#' @param threshold_fishing_time {\link[base]{numeric}} expected. Default values: 13. Maximum valid time for fishing time
#' @param vessel_activity_sea_time {\link[base]{character}} expected. Default values: c("1", "2", "3", "4", "6", "6", "8", "9", "10", "11", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "29", "30", "31", "32", "36", "37", "38", "39", "50", "99", "101", "101", "102", "102", "103"). Code activities. First criterion for identifying activities that must have sea time
#' @param object_operation_sea_time {\link[base]{character}} expected. Default values: c(NA, NA, NA, NA, "99", NA, NA, NA, NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA, "99", NA, NA). Code object operation. Second criterion for identifying activities that must have sea time (Indicate NA if you want the code object operation to be missing)
#' @param vessel_activity_fishing_time {\link[base]{character}} expected. Default values: c("2", "3", "4", "6", '6', "8", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "19", "20", "23", "25", "26", "27", "29", "30", "31", "102", "102"). Code activities. First criterion for identifying activities that must have fishing time
#' @param object_operation_fishing_time {\link[base]{character}} expected. Default values: c(NA, NA, NA, "99", NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA). Code object operation. Second criterion for identifying activities that must have fishing time (Indicate NA if you want the code object operation to be missing)
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  route_id}}
#'  \item{\code{  route_seatime}}
#'  \item{\code{  route_fishingtime}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  route_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Day 1 is ok,
#' #Day 2 has a fishing time equal to 0 but includes 1 fishing activitie
#' #Day 3 has a sea time equal to 0 but includes 1 activitie on the sea
#' #Day 4 has a missing sea time,
#' #Day 5 has a missing fishing time,
#' #Day 6 has a sea time greater than threshold (threshold_sea_time),
#' #Day 7 has a fishing time greater than threshold (threshold_fishing_time),
#' #Day 8 has a fishing time greater than sea time
#' dataframe1 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          route_seatime = c(6, 2, 0, NA, 8, 26, 15, 8),
#'                          route_fishingtime = c(5, 0, 2, 4, NA, 7, 14, 9))
#' dataframe2 <- data.frame(activity_id = c("1", "2", "3", "4", "5"),
#'                          vesselactivity_code = c("2","13", "103","15", "6"),
#'                          route_id = c("1", "1", "2", "2", "3"))
#' dataframe3 <- data.frame(floatingobject_id = c("1"),
#'                          objectoperation_code = c("1"),
#'                          activity_id = c("2"))
#' @expect equal(., structure(list(route_id = c("1", "2", "3", "4", "5", "6", "7", "8"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), route_seatime = c(6, 2, 0, NA, 8, 26, 15, 8), route_fishingtime = c(5, 0, 2, 4, NA, 7, 14, 9), nb_activity_must_seatime = c(2, 2, 1, 0, 0, 0, 0, 0), nb_activity_must_fishingtime = c(2, 1, 1, 0, 0, 0, 0, 0)), row.names = c(NA, 8L), class = "data.frame"))
#' check_time_route_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_time_route_inspector <- function(dataframe1,
                                       dataframe2,
                                       dataframe3,
                                       output,
                                       threshold_sea_time = 24,
                                       threshold_fishing_time = 13,
                                       vessel_activity_sea_time = c("1", "2", "3", "4", "6", "6", "8", "9", "10", "11", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "29", "30", "31", "32", "36", "37", "38", "39", "50", "99", "101", "101", "102", "102", "103"),
                                       object_operation_sea_time = c(NA, NA, NA, NA, "99", NA, NA, NA, NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA, "99", NA, NA),
                                       vessel_activity_fishing_time = c("2", "3", "4", "6", "6", "8", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "19", "20", "23", "25", "26", "27", "29", "30", "31", "102", "102"),
                                       object_operation_fishing_time = c(NA, NA, NA, "99", NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA)) {
  # 0 - Global variables assignement ----
  route_id <- NULL
  route_seatime <- NULL
  route_fishingtime <- NULL
  activity_id <- NULL
  threshold <- NULL
  logical_activity_seatime <- NULL
  logical_activity_fishingtime <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("route_id", "route_seatime", "route_fishingtime"),
    column_type = c("character", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("route_id", "route_seatime", "route_fishingtime"),
      column_type = c("character", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("route_id", "route_seatime", "route_fishingtime")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code", "route_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code", "route_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "vesselactivity_code", "route_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("floatingobject_id", "objectoperation_code", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_sea_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_sea_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_fishing_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_fishing_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (length(vessel_activity_sea_time) != length(object_operation_sea_time)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"vessel_activity_sea_time\", \"object_operation_sea_time\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity_sea_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity_sea_time,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = object_operation_sea_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_operation_sea_time,
      type = "character",
      output = "error"
    ))
  }
  if (length(vessel_activity_fishing_time) != length(object_operation_fishing_time)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"vessel_activity_fishing_time\", \"object_operation_fishing_time\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity_fishing_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity_fishing_time,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = object_operation_fishing_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_operation_fishing_time,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$route_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare sea time and the threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold_sea_time
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_sea_threshold <- codama::vector_comparison(
    first_vector = dataframe1$route_seatime,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  # Compare fishing time and the threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold_fishing_time
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_fishing_threshold <- codama::vector_comparison(
    first_vector = dataframe1$route_fishingtime,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison_sea_threshold$logical & comparison_fishing_threshold$logical
  # Sea time must be equal to or greater than fishing time
  dataframe1[!is.na(dataframe1$route_fishingtime) & dataframe1$route_fishingtime > 0 & !is.na(dataframe1$route_seatime) & dataframe1$route_seatime < dataframe1$route_fishingtime, "logical"] <- FALSE
  # Merge
  data_route_activity_objectoperation <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(route_id))
  data_route_activity_objectoperation <- dplyr::left_join(data_route_activity_objectoperation, dataframe3, by = dplyr::join_by(activity_id))
  # Sea time category conditions
  condition_seatime <- as.list(as.data.frame(t(data.frame(vessel_activity_sea_time, object_operation_sea_time))))
  # Selection of activities that must have sea time
  activity_seatime <- purrr::map(condition_seatime, ~ data_route_activity_objectoperation %>%
                                   dplyr::filter((vesselactivity_code == .x[1] | (is.na(vesselactivity_code) & is.na(.x[1]))) & (objectoperation_code == .x[2] | (is.na(objectoperation_code) & is.na(.x[2])))))
  activity_seatime <- dplyr::bind_rows(activity_seatime)
  # Count the number of activities requiring time at sea per route
  activity_seatime <- activity_seatime %>%
    dplyr::group_by(route_id, route_seatime) %>%
    dplyr::summarise(nb_activity_must_seatime = length(unique(activity_id)), logical_activity_seatime = FALSE, .groups = "drop")
  # Time at sea per route are correct
  activity_seatime[!is.na(activity_seatime$route_seatime) & activity_seatime$route_seatime > 0, "logical_activity_seatime"] <- TRUE
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, activity_seatime, by = dplyr::join_by(route_id, route_seatime))
  dataframe1[is.na(dataframe1$nb_activity_must_seatime), "nb_activity_must_seatime"] <- 0
  dataframe1[is.na(dataframe1$logical_activity_seatime), "logical_activity_seatime"] <- TRUE
  # Fishing time time category conditions
  condition_fishingtime <- as.list(as.data.frame(t(data.frame(vessel_activity_fishing_time, object_operation_fishing_time))))
  # Selection of activities that must have fishing time
  activity_fishingtime <- purrr::map(condition_fishingtime, ~ data_route_activity_objectoperation %>%
                                       dplyr::filter((vesselactivity_code == .x[1] | (is.na(vesselactivity_code) & is.na(.x[1]))) & (objectoperation_code == .x[2] | (is.na(objectoperation_code) & is.na(.x[2])))))
  activity_fishingtime <- dplyr::bind_rows(activity_fishingtime)
  # Count the number of activities requiring time at sea per route
  activity_fishingtime <- activity_fishingtime %>%
    dplyr::group_by(route_id, route_fishingtime) %>%
    dplyr::summarise(nb_activity_must_fishingtime = length(unique(activity_id)), logical_activity_fishingtime = FALSE, .groups = "drop")
  # Fishing time per route are correct
  activity_fishingtime[!is.na(activity_fishingtime$route_fishingtime) & activity_fishingtime$route_fishingtime > 0, "logical_activity_fishingtime"] <- TRUE
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, activity_fishingtime, by = dplyr::join_by(route_id, route_fishingtime))
  dataframe1[is.na(dataframe1$nb_activity_must_fishingtime), "nb_activity_must_fishingtime"] <- 0
  dataframe1[is.na(dataframe1$logical_activity_fishingtime), "logical_activity_fishingtime"] <- TRUE
  dataframe1$logical <- dataframe1$logical & dataframe1$logical_activity_seatime & dataframe1$logical_activity_fishingtime
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(threshold, logical_activity_seatime, logical_activity_fishingtime))
  dataframe1 <- dplyr::relocate(.data = dataframe1, route_seatime, route_fishingtime, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$route_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$route_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " routes with sea time or fishing time inconsistency", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_eez_inspector
#' @title Gives the inconsistencies between the fishing area declared and calculated for the activity
#' @description The purpose of the check_eez_inspector function is to provide a table of data that contains an inconsistency between the fishing area declared and calculated with position for the activity fishing
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weight_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Layer to containing the eez shapefile (example cf : Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632)
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param international_waters_code {\link[base]{character}} expected. Default values: "XIN". iso3 code corresponding to international waters
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector of inventory of codes for activities that must have a zee zone
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  fpazone_code}}
#'  \item{\code{  fpazone_country_iso3}}
#'  \item{\code{  activity_position}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  ISO_TER1}}
#'  \item{\code{  ISO_TER2}}
#'  \item{\code{  ISO_TER3}}
#'  \item{\code{  geometry}}
#' }
#' @doctest
#' #Activity 1, 2, 3 and 5 are ok,
#' #Activity 4 is outside the EEZ delimited in the shapefile,
#' #Activity 6 has a missing EEZ zone and the vessel's activity is in vessel_activity,
#' #Activity 7 has different EEZ,
#' #Activity 8 has an EEZ zone unknown to the shapefile and which also does not exist in
#' #           international_waters_code
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"),
#'                          fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"),
#'                          fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"),
#'                          activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)",
#'                                                "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)",
#'                                                "POINT (1 1)", "POINT (6 6)"))
#' dataframe2 <- sf::st_sf(data.frame(ISO_TER1 = c("SYC", "XSG"),
#'                                    ISO_TER2 = c(NA, NA),
#'                                    ISO_TER3 = c(NA, NA),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          sf::st_polygon(list(rbind(c(3,3), c(3,5),
#'                                                                                    c(5,5), c(5,3),
#'                                                                                    c(3,3)))),
#'                                                          crs = 4326)))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), eez_calculated = c("SYC", "XSG", NA, "On land or in international waters", NA, NA, "SYC", "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA, 8L), class = "data.frame"), structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)", "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)", "POINT (1 1)", "POINT (6 6)"), eez_calculated = c("SYC", "XSG", NA, "On land or in international waters", NA, NA, "SYC", "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, 8L), class = "data.frame")))
#' check_eez_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_eez_inspector <- function(dataframe1,
                                dataframe2,
                                output,
                                activity_crs = 4326,
                                international_waters_code = "XIN",
                                vessel_activity = c("6")) {
  # 0 - Global variables assignement ----
  activity_position <- NULL
  activity_id <- NULL
  fpazone_code <- NULL
  fpazone_country_iso3 <- NULL
  ISO_TER1 <- NULL
  ISO_TER2 <- NULL
  ISO_TER3 <- NULL
  logical_eez <- NULL
  filter_activity_geo <- NULL
  eez_calculated <- NULL
  filter_international_waters <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position"),
    column_type = c("character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position"),
      column_type = c("character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = international_waters_code,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = international_waters_code,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Checks for the presence of a declared fishing zone
  dataframe1$logical <- !c(is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3))
  # Formats spatial data activity
  dataframe1$filter_activity_geo <- !is.na(dataframe1$activity_position) & !c(is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3))
  data_geo_activity <- dataframe1 %>%
    dplyr::filter(filter_activity_geo) %>%
    sf::st_as_sf(wkt = "activity_position", crs = activity_crs) %>%
    sf::st_transform(activity_position, crs = 4326)
  # Calculates the intersection between activity and eez
  if (nrow(data_geo_activity) > 0) {
    intersect_eez <- terra::intersect(data_geo_activity %>%
                                        terra::vect(), dataframe2 %>%
                                        terra::vect()) %>%
      sf::st_as_sf() %>%
      sf::st_drop_geometry()
    # Compares declared country with calculated country
    intersect_eez <- intersect_eez %>%
      dplyr::group_by(activity_id) %>%
      dplyr::summarise(logical_eez = any(c(fpazone_code, fpazone_country_iso3) %in% c(ISO_TER1, ISO_TER2, ISO_TER3)),
                       eez_calculated = paste0(unique(c(ISO_TER1, ISO_TER2, ISO_TER3))[!is.na(unique(c(ISO_TER1, ISO_TER2, ISO_TER3)))], collapse = ", "))
    # Merge
    dataframe1 <- dplyr::left_join(dataframe1, intersect_eez, by = dplyr::join_by(activity_id))
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_eez <- NA
      dataframe1$eez_calculated <- NA
    } else {
      dataframe1$logical_eez <- logical()
      dataframe1$eez_calculated <- character()
    }
  }
  # Case of international waters : the calculated country must be missing
  dataframe1$filter_international_waters <- ((dataframe1$fpazone_code %in% international_waters_code & dataframe1$fpazone_country_iso3 %in% international_waters_code) | (is.na(dataframe1$fpazone_code) & dataframe1$fpazone_country_iso3 %in% international_waters_code) | (dataframe1$fpazone_code %in% international_waters_code & is.na(dataframe1$fpazone_country_iso3)))
  dataframe1[is.na(dataframe1$logical_eez) & dataframe1$filter_international_waters, "logical_eez"] <- TRUE
  # Case of no calculated country
  dataframe1[is.na(dataframe1$logical_eez), "logical_eez"] <- FALSE
  dataframe1$logical <- dataframe1$logical & dataframe1$logical_eez
  dataframe1[is.na(dataframe1$eez_calculated) & dataframe1$filter_activity_geo & !dataframe1$filter_international_waters, "eez_calculated"] <- "On land or in international waters"
  # Case of vessel_activity with no constraints if missing declared fishing zone
  dataframe1$logical[!(dataframe1$vesselactivity_code %in% vessel_activity) & is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3)] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, eez_calculated, .before = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_eez, filter_activity_geo, filter_international_waters))
  activity_data_detail <- dataframe1
  activity_data_detail <- activity_data_detail %>%
    dplyr::mutate(activity_crs = activity_crs)
  dataframe1 <- subset(dataframe1, select = -c(activity_position))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with declared eez that do not correspond to calculated eez")))
  }
  if (output == "report") {
    return(list(dataframe1, activity_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_species_inspector
#' @title Gives the inconsistencies between species sampled and species authorized
#' @description The purpose of the check_species_inspector function is to provide a table of data that contains an inconsistency between the species sampled and species authorized
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_species_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). Vector of the species authorized.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#' }
#' @doctest
#' #Sample species 1 is ok,
#' #Sample species 2 has a species outside the list species
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2"),
#'                          species_fao_code = c("YFT", "JOS"))
#' @expect equal(., structure(list(samplespecies_id = c("1", "2"), logical = c(TRUE, FALSE), species_fao_code = c("YFT", "JOS")), row.names = c(NA, -2L), class = "data.frame"))
#' check_species_inspector(dataframe1, output = "report")
#' @export
check_species_inspector <- function(dataframe1,
                                    output,
                                    species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT")) {
  # 0 - Global variables assignement ----
  species_fao_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "species_fao_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare specie of the samples
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison_species$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, species_fao_code, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with species not included in the authorized list (", paste0(species, collapse = ", "), ")", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_sample_without_measure_inspector
#' @title Gives inconsistencies between the sample and the measurement in terms of presence
#' @description The purpose of the check_sample_without_measure_inspector function is to provide a table of data that contains an inconsistency between the sample and the presence of measurement
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_without_measure_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_without_measure_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample species 1 is ok,
#' #Sample species 2 has no sample species measure
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2"))
#' dataframe2 <- data.frame(samplespeciesmeasure_id = c("1", "2"),
#'                          samplespecies_id = c("1", "1"))
#' @expect equal(., structure(list(samplespecies_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' check_sample_without_measure_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sample_without_measure_inspector <- function(dataframe1,
                                                   dataframe2,
                                                   output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespeciesmeasure_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespeciesmeasure_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "samplespeciesmeasure_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search samplespeciesmeasure ID in the associations samplespecies ID
  dataframe2 <- dataframe2[!is.na(dataframe2$samplespeciesmeasure_id), ]
  # Check
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$samplespecies_id,
    second_vector = dataframe2$samplespecies_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples species with no measurement", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_sample_without_species_inspector
#' @title Gives inconsistencies between the sample and the species sampled in terms of presence
#' @description The purpose of the check_sample_without_species_inspector function is to provide a table of data that contains an inconsistency between the sample and the presence of species
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_without_species_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_without_species_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  sample_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 is ok,
#' #Sample 2 has no sample species
#' dataframe1 <- data.frame(sample_id = c("1", "2"))
#' dataframe2 <- data.frame(samplespecies_id = c("1", "2"),
#'                          sample_id = c("1", "1"))
#' @expect equal(., structure(list(sample_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' check_sample_without_species_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sample_without_species_inspector <- function(dataframe1,
                                                   dataframe2,
                                                   output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "samplespecies_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "samplespecies_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "samplespecies_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search samplespecies ID in the associations samples ID
  dataframe2 <- dataframe2[!is.na(dataframe2$samplespecies_id), ]
  # Check
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$sample_id,
    second_vector = dataframe2$sample_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with no species", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_super_sample_number_consistent_inspector
#' @title Gives the inconsistencies between the sample and the subsample number
#' @description The purpose of the check_super_sample_number_consistent_inspector function is to provide a table of data that contains an inconsistency between the sample and the subsample number
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_super_sample_number_consistent_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_super_sample_number_consistent_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_supersample}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespecies_subsamplenumber}}
#'  \item{\code{  sample_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 is not a super sample but the numbering starts at 1 instead of 0,
#' #Sample 4 is a super sample but the same sub-sample number appears twice,
#' #Sample 5 has no sample species,
#' #Sample 6 is a super sample but the numbering starts at 0 instead of 1,
#' #Sample 7 is a super sample but has only one sub-sample species
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          sample_supersample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
#' dataframe2 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'                          samplespecies_subsamplenumber = c(0, 1, 2, 1, 1, 1, 0, 1, 1),
#'                          sample_id = c("1", "2", "2", "3", "4", "4", "6", "6", "7"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), sample_supersample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), count_subsamplenumber_n0 = c(0L, 2L, 1L, 2L, NA, 1L, 1L), count_subsamplenumber_0 = c(1L, 0L, 0L, 0L, NA, 1L, 0L), count_subsamplenumber_1 = c(0L, 1L, 1L, 2L, NA, 1L, 1L), count_subsamplenumber = c(1L, 2L, 1L, 1L, NA, 2L, 1L)), row.names = c(NA, 7L), class = "data.frame"))
#' check_super_sample_number_consistent_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_super_sample_number_consistent_inspector <- function(dataframe1,
                                                           dataframe2,
                                                           output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  samplespecies_subsamplenumber <- NULL
  samplespecies_id <- NULL
  count_subsamplenumber_n0 <- NULL
  count_subsamplenumber_0 <- NULL
  count_samplespecies <- NULL
  count_subsamplenumber_1 <- NULL
  only_one_subsampling <- NULL
  many_subsampling <- NULL
  count_samplespecies_bis <- NULL
  count_subsamplenumber_n0_bis <- NULL
  count_subsamplenumber_0_bis <- NULL
  count_subsamplenumber_1_bis <- NULL
  sample_supersample <- NULL
  count_subsamplenumber <- NULL
  count_subsamplenumber_bis <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_supersample"),
    column_type = c("character", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_supersample"),
      column_type = c("character", "logical"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_supersample")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search subsample number in the associations samples ID
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count_subsamplenumber_n0 = sum(samplespecies_subsamplenumber != 0), count_subsamplenumber_0 = sum(samplespecies_subsamplenumber == 0), count_samplespecies = sum(!is.na(unique(samplespecies_id))), count_subsamplenumber = sum(!is.na(unique(samplespecies_subsamplenumber))), count_subsamplenumber_1 = sum(samplespecies_subsamplenumber == 1))
  # Merge
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- TRUE
  } else {
    dataframe1$logical <- logical()
  }
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  # Case of NA count_subsamplenumber_n0, count_subsamplenumber_0, count_samplespecies or count_subsamplenumber_1
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      count_subsamplenumber_n0_bis = dplyr::coalesce(count_subsamplenumber_n0, 0),
      count_subsamplenumber_0_bis = dplyr::coalesce(count_subsamplenumber_0, 0),
      count_samplespecies_bis = dplyr::coalesce(count_samplespecies, 0),
      count_subsamplenumber_bis = dplyr::coalesce(count_subsamplenumber, 0),
      count_subsamplenumber_1_bis = dplyr::coalesce(count_subsamplenumber_1, 0),
    )
  dataframe1[dataframe1$count_samplespecies_bis == 0, "logical"] <- FALSE
  dataframe1$only_one_subsampling <- dataframe1$sample_supersample == FALSE & dataframe1$count_subsamplenumber_n0_bis == 0
  dataframe1$many_subsampling <- dataframe1$sample_supersample == TRUE & dataframe1$count_subsamplenumber_0_bis == 0 & dataframe1$count_subsamplenumber_bis > 1
  dataframe1[!(dataframe1$only_one_subsampling | dataframe1$many_subsampling), "logical"] <- FALSE
  dataframe1[dataframe1$count_subsamplenumber_bis == 1 & dataframe1$count_subsamplenumber_1_bis > 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(only_one_subsampling, many_subsampling, count_samplespecies_bis, count_subsamplenumber_bis, count_subsamplenumber_n0_bis, count_subsamplenumber_0_bis, count_subsamplenumber_1_bis, count_samplespecies))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_supersample, count_subsamplenumber_n0, count_subsamplenumber_0, count_subsamplenumber_1, count_subsamplenumber, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with subsample number", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_well_number_consistent_inspector
#' @title Gives the inconsistencies between sample well number and associated trip well numbers
#' @description The purpose of the check_well_number_consistent_inspector  function is to provide a table of data that contains an inconsistency between sample well number and associated trip well numbers
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_well_number_consistent_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_well_number_consistent_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_well_number_consistent_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("5", "6", "10"). Vector of codes for vessel types with a well plan.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  well_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  well_label}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vesseltype_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, 2 and 3 are ok,
#' #Sample 4 has different well,
#' #Sample 5 has no well
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5"),
#'                          sample_well = c("well_1", "well_2", "well_2", "well_2", NA),
#'                          trip_id = c("1", "1", "2", "3", "3"))
#' dataframe2 <- data.frame(well_id = c("1", "2", "3"),
#'                          trip_id = c("1", "1", "3"),
#'                          well_label = c("well_1", "well_2", "well_1"))
#' dataframe3 <- data.frame(trip_id = c("1", "2", "3"),
#'                          vesseltype_code = c("6", "1", "6"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), sample_well = c("well_1", "well_2", "well_2", "well_2", NA)), row.names = c(NA, 5L), class = "data.frame"))
#' check_well_number_consistent_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_well_number_consistent_inspector <- function(dataframe1,
                                                   dataframe2,
                                                   dataframe3,
                                                   output,
                                                   vessel_type = c("5", "6", "10")) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  sample_well <- NULL
  well_id <- NULL
  vesseltype_code <- NULL
  well_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_well", "trip_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_well", "trip_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("well_id", "trip_id", "well_label"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("well_id", "trip_id", "well_label"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("well_id", "trip_id", "well_label")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("trip_id", "vesseltype_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("trip_id", "vesseltype_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  if (nrow(dataframe2) > 0) {
    dataframe2$logical <- TRUE
  } else {
    dataframe2$logical <- logical()
  }
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id == trip_id, sample_well == well_label))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(trip_id))
  # Search well not link
  dataframe1[is.na(dataframe1$logical), "logical"] <- FALSE
  # Case the well number is empty
  dataframe1[is.na(dataframe1$sample_well), "logical"] <- FALSE
  # Vessel types without a well plan
  dataframe1[!(dataframe1$vesseltype_code %in% vessel_type), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, well_id, vesseltype_code))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_well, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with well number", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_little_big_inspector
#' @title Gives the inconsistencies between the percentage of little and big fish sampled
#' @description The purpose of the check_little_big_inspector function is to provide a table of data that contains an inconsistency between the percentage of little and big fish sampled
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species_big {\link[base]{character}} expected. Default values:  c("YFT", "YFT", "BET", "BET", "ALB", "ALB"). Vector of the species. First criterion for identifying big fish (other values are small fish)
#' @param size_measure_type_big {\link[base]{character}} expected. Default values: c("PD1", "FL", "PD1", "FL", "PD1", "FL"). Vector of the size measure type. Second criterion for identifying big fish (other values are small fish)
#' @param threshold_size_big {\link[base]{numeric}} expected. Default values: c(24, 80, 25, 77, 23.5, 78). Vector for defining the lower or equal for threshold size measurement. Third criterion for identifying big fish (other values are small fish)
#' @param size_measure_type {\link[base]{character}} expected. Default values: c("FL", "PD1"). Vector with the preferred type of size measurement for small fish and then for big fish
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.9. Threshold for percentage of small or big fish
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, 2, 4, 5, 7, 8 and 9 are ok,
#' #Sample 3 has a percentage of big below the threshold,
#' #Sample 6 has a percentage of little below the threshold,
#' #Sample 10 has a percentage of little below the threshold,
#' #Sample 11 has a percentage of little below the threshold,
#' #Sample 12 has a percentage of big below the threshold
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                        "12"),
#'                          sample_smallsweight = c(10, 20, 1, 30, 3, 7, 4, 12, 0, 0, 3, 0),
#'                          sample_bigsweight = c(NA, 2, 9, 3, 5, 4, 13, 5, 0, NA, 0, 4),
#'                          sample_totalweight = c(NA, NA, NA, 33, NA, NA, 7, 2, 0, 5, 0, 0))
#' dataframe2 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12"),
#'                          species_fao_code = c("YFT", "BET", "BET", "YFT", "ALB", "YFT", "YFT",
#'                                               "BET", "YFT", "ALB", "YFT", "YFT"),
#'                          sizemeasuretype_code = c("PD1", "PD1", "FL","PD1", "FL", "FL", "PD1",
#'                                                   "FL", "PD1", "PD1", "PD1", "PD1"),
#'                          sample_id = c("1", "2", "2", "3", "4", "5", "5", "6", "6", "8", "9", "10"))
#' dataframe3 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
#'                                                      "10", "11", "12"),
#'                          samplespeciesmeasure_sizeclass = c(20, 26, 70, 20, 10, 45, 36, 30, 32, 24,
#'                                                             13, 13),
#'                          samplespeciesmeasure_count = c(5, 1, 9, 8, 10, 25, 2, 16, 4, 3, 6, 6),
#'                          samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12"))
#' @expect equal(., structure(list(sample_id = c("1", "10", "11", "12", "2", "3", "4", "5", "6", "7", "8", "9"), logical = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), sample_smallsweight = c(10, 0, 3, 0, 20, 1, 30, 3, 7, 4, 12, 0), sample_bigsweight = c(NA, NA, 0, 4, 2, 9, 3, 5, 4, 13, 5, 0), sample_totalweight = c(NA, 5, 0, 0, NA, NA, 33, NA, NA, 7, 2, 0), little_percentage = c(1, 1, 0, 0, 0.9, 1, 1, 0.925925925925926, 0.8, 0, 0, 1), big_percentage = c(0, 0, 0, 0, 0.1, 0, 0, 0.0740740740740741, 0.2, 0, 1, 0), measure1_percentage = c(0, 0, 0, 0, 0.9, 0, 1, 0.925925925925926, 0.8, 0, 0, 0), measure2_percentage = c(1, 1, 0, 0, 0.1, 1, 0, 0.0740740740740741, 0.2, 0, 1, 1)), row.names = c(NA, -12L), class = "data.frame"))
#' check_little_big_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_little_big_inspector <- function(dataframe1,
                                       dataframe2,
                                       dataframe3,
                                       output,
                                       species_big = c("YFT", "YFT", "BET", "BET", "ALB", "ALB"),
                                       size_measure_type_big = c("PD1", "FL", "PD1", "FL", "PD1", "FL"),
                                       threshold_size_big = c(24, 80, 25, 77, 23.5, 78),
                                       size_measure_type = c("FL", "PD1"),
                                       threshold = 0.9) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  sample_totalweight <- NULL
  species_fao_code <- NULL
  samplespeciesmeasure_count <- NULL
  sizemeasuretype_code <- NULL
  sample_smallsweight_bis <- NULL
  sample_bigsweight_bis <- NULL
  sample_totalweight_bis <- NULL
  little_percentage <- NULL
  big_percentage <- NULL
  measure1_percentage <- NULL
  measure2_percentage <- NULL
  samplespecies_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id"),
    column_type = c("character", "numeric", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id"),
      column_type = c("character", "numeric", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_size_big,
    type = "numeric",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_size_big,
      type = "numeric",
      output = "error"
    ))
  }
  if (length(species_big) != length(size_measure_type_big) || length(species_big) != length(threshold_size_big)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"species_big\", \"size_measure_type_big\" and \"threshold_size_big\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type,
    type = "character",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type,
      type = "character",
      length = 2L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(samplespecies_id))
  # Calculate the number of the measure per sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  total_count <- dataframe1 %>%
    dplyr::group_by(sample_id, sample_smallsweight, sample_bigsweight, sample_totalweight) %>%
    dplyr::summarise(total_count = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)), .groups = "drop")
  # Small and large category conditions
  condition <- as.list(as.data.frame(t(data.frame(species_big, size_measure_type_big, threshold_size_big))))
  # Measurement selection of small individuals
  little <- purrr::map(condition, ~ dataframe1 %>%
                         dplyr::filter(species_fao_code == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass < as.numeric(.x[3])))
  little <- dplyr::bind_rows(little)
  little <- dataframe1 %>%
    dplyr::filter(!(species_fao_code %in% species_big)) %>%
    dplyr::bind_rows(little)
  # Calculation of the number of measurements of small individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  little <- little %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(little = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Measurement selection of big individuals
  big <- purrr::map(condition, ~ dataframe1 %>%
                      dplyr::filter(species_fao_code == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass >= as.numeric(.x[3])))
  big <- dplyr::bind_rows(big)
  # Calculation of the number of measurements of big individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  big <- big %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(big = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 1 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == size_measure_type[1]) %>%
    dplyr::summarise(measure1 = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 2 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure2 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == size_measure_type[2]) %>%
    dplyr::summarise(measure2 = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Merge
  total_count <- dplyr::left_join(total_count, little, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, big, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, measure1, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, measure2, by = dplyr::join_by(sample_id))
  # Calculates percentages
  total_count <- total_count %>%
    dplyr::mutate(little_percentage = little / total_count, big_percentage = big / total_count, measure1_percentage = measure1 / total_count, measure2_percentage = measure2 / total_count)
  # Case of NA sample_smallsweight, sample_bigsweight or sample_totalweight
  total_count <- total_count %>%
    dplyr::mutate(
                  sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
                  sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
                  sample_totalweight_bis = dplyr::coalesce(sample_totalweight, 0))
  # Case of NA little_percentage, big_percentage, measure1_percentage, measure2_percentage
  total_count[is.na(total_count$little_percentage), "little_percentage"] <- 0
  total_count[is.na(total_count$big_percentage), "big_percentage"] <- 0
  total_count[is.na(total_count$measure1_percentage), "measure1_percentage"] <- 0
  total_count[is.na(total_count$measure2_percentage), "measure2_percentage"] <- 0
  # Check
  total_count$logical <- !((total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis == 0 & total_count$little_percentage < threshold) |
                             ((total_count$sample_totalweight_bis != 0 | (total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis > 0)) & total_count$measure1_percentage > total_count$measure2_percentage & total_count$little_percentage < threshold) |
                             (total_count$sample_smallsweight_bis == 0 & total_count$sample_bigsweight_bis > 0 & total_count$big_percentage < threshold) |
                             ((total_count$sample_totalweight_bis != 0 | (total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis > 0)) & total_count$measure1_percentage < total_count$measure2_percentage & total_count$big_percentage < threshold))
  # Case of NA
  total_count[is.na(total_count$logical), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  total_count <- subset(total_count, select = -c(total_count, little, big, measure1, measure2, sample_smallsweight_bis, sample_bigsweight_bis, sample_totalweight_bis))
  total_count <- dplyr::relocate(.data = total_count, sample_smallsweight, sample_bigsweight, sample_totalweight, little_percentage, big_percentage, measure1_percentage, measure2_percentage, .after = logical) %>%
    data.frame()
  if ((sum(total_count$logical, na.rm = TRUE) + sum(!total_count$logical, na.rm = TRUE)) != nrow_first || any(is.na(total_count$logical))) {
    all <- c(select, total_count$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(total_count$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(total_count$logical)), "):", paste0(total_count$sample_id[is.na(total_count$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!total_count$logical), " samples inconsistency with percentage of little and big fish", collapse = ", ")))
  }
  if (output == "report") {
    return(total_count)
  }
  if (output == "logical") {
    if (sum(!total_count$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_weighting_inspector
#' @title Gives the inconsistencies between the sample weighting and sample weight or landed weight
#' @description The purpose of the check_weighting_inspector  function is to provide a table of data that contains an inconsistency between the sample weighting
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("6", "2"). List of two elements, the first is the seine vessel type code, and the second is the baitboat type code.
#' @param threshold_weight {\link[base]{numeric}} expected. Default values: 100. Seiner threshold weight
#' @param threshold_ratio {\link[base]{numeric}} expected. Default values: 0.95. Percentage threshold between weight and weighted weight for seiners
#' @param sample_type_code_landing_baitboat {\link[base]{character}} expected. Default values: c("11"). List of sample type codes for baitboat fresh landings
#' @param landing_type_baitboat {\link[base]{character}} expected. Default values: c("L-YFT-10", "L-BET-10", "L-TUN-10"). List of codes for fresh baitboat landings
#' @param threshold_baitboat {\link[base]{numeric}} expected. Default values: 1. Threshold for baitboats with respect to the difference between the weighted weight and the landed fresh weight and the difference between the weight and the weighted weight
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  sampletype_code}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vesseltype_code}}
#'  \item{\code{  vesseltype_label1}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  weightcategory_code}}
#'  \item{\code{  trip_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, 3, 4, 7, 9, 12, 17 and 18 are ok,
#' #Sample 2 has a weighted weight ratio over the sum of the weights of small and big individuals below
#' #         the threshold (threshold_ratio),
#' #Sample 5 has a difference between fresh landing and weighted weight above the
#' #         threshold (threshold_baitboat),
#' #Sample 6 has no vessel type,
#' #Sample 8 has no sample type,
#' #Sample 10 has no sample activity,
#' #Sample 11 has a difference between total weight and weighted weight above the
#' #          threshold (threshold_baitboat),
#' #Sample 13 has no sample activity,
#' #Sample 14 has a difference between sum of the weights of small and big individuals and weighted
#' #          weight above the threshold (threshold_baitboat),
#' #Sample 15 has the sum of the weights of small and big individuals above the
#' #          threshold (threshold_weight),
#' #Sample 16 has the sum of the total weight above the threshold (threshold_weight),
#' #Sample 19 has a weighted weight ratio over the total weight below the threshold (threshold_ratio)
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                        "12", "13", "14", "15", "16", "17", "18", "19"),
#'                          sample_smallsweight = c(10, 32, 2.5, 30, 12, 7, NA, 6, NA, 4, 8, 3, 7, 13,
#'                                                  54, 3, 8, 2, 16),
#'                          sample_bigsweight = c(50, 2, 9, 3, 6, 13, 0, 3, 7, 2, 0, 2, 8, 3, 62, 8,
#'                                                15, 6, 1),
#'                          sample_totalweight = c(NA, NA, NA, 33, 8, 9, 142, 2, 14, 10, 3, 0, NA, 0,
#'                                                 0, 104, 24, 36, 12),
#'                          trip_id =  c("1", "1", "2", "3", "4", "5", "6", "7", "8", "8", "8", "8",
#'                                       "8", "8", "1", "1", "1", "1", "1"),
#'                          sampletype_code = c("1", "1", "1", "11", "11", "1", "1", NA, "1", "1", "1",
#'                                              "1", "1", "1", "1", NA, NA, NA, NA))
#' dataframe2 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12", "13", "14", "15", "16", "17"),
#'                          sampleactivity_weightedweight = c(70, 5, 18, 12, 33, 5, 9, 4, 13, 7, 4, 15,
#'                                                            116, 104, 24, 35, 11),
#'                          sample_id = c("1", "1", "2", "3", "4", "5", "6", "8", "9", "11", "12",
#'                                        "14", "15", "16", "17", "18", "19"))
#' dataframe3 <- data.frame(trip_id = c("1", "2", "3", "4", "6", "7", "8"),
#'                          vesseltype_code = c("6", "6", "2", "2", "3", "2", "2"),
#'                          vesseltype_label = c("vessel_type_1", "vessel_type_1", "vessel_type_2",
#'                                               "vessel_type_2", "vessel_type_3", "vessel_type_2",
#'                                               "vessel_type_2"))
#' dataframe4 <- data.frame(landing_id = c("1", "2", "3", "4", "5", "6"),
#'                          landing_weight = c(85, 26, 30, 2.6, 20, 3),
#'                          weightcategory_code = c("W-1", "W-1", "L-YFT-10", "L-YFT-10", "L-YFT-10",
#'                                                  "L-BET-10"),
#'                          trip_id = c("1", "2", "3", "3", "4", "7"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), sample_smallsweight = c(10, 32, 2.5, 30, 12, 7, NA, 6, NA, 4, 8, 3, 7, 13, 54, 3, 8, 2, 16), sample_bigsweight = c(50, 2, 9, 3, 6, 13, 0, 3, 7, 2, 0, 2, 8, 3, 62, 8, 15, 6, 1), sample_totalweight = c(NA, NA, NA, 33, 8, 9, 142, 2, 14, 10, 3, 0, NA, 0, 0, 104, 24, 36, 12), sampletype_code = c("1", "1", "1", "11", "11", "1", "1", NA, "1", "1", "1", "1", "1", "1", "1", NA, NA, NA, NA), weightedweight = c(75, 18, 12, 33, 5, 9, NA, 4, 13, NA, 7, 4, NA, 15, 116, 104, 24, 35, 11), vesseltype_label = c("vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_2", "vessel_type_2", NA, "vessel_type_3", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_1"), sum_landing_weight_baitboat = c(NA, NA, NA, 32.6, 20, NA, NA, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA, -19L), class = "data.frame"))
#' check_weighting_inspector(dataframe1, dataframe2, dataframe3, dataframe4, output = "report")
#' @export
check_weighting_inspector <- function(dataframe1,
                                      dataframe2,
                                      dataframe3,
                                      dataframe4,
                                      output,
                                      vessel_type = c("6", "2"),
                                      threshold_weight = 100,
                                      threshold_ratio = 0.95,
                                      sample_type_code_landing_baitboat = c("11"),
                                      landing_type_baitboat = c("L-YFT-10", "L-BET-10", "L-TUN-10"),
                                      threshold_baitboat = 1) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  sample_totalweight <- NULL
  weight_calculation <- NULL
  sampleactivity_weightedweight <- NULL
  trip_id <- NULL
  weightcategory_code <- NULL
  landing_weight <- NULL
  weightedweight <- NULL
  sum_landing_weight_baitboat <- NULL
  weight <- NULL
  vesseltype_code <- NULL
  weightedweight_bis <- NULL
  sum_landing_weight_baitboat_bis <- NULL
  sampletype_code <- NULL
  vesseltype_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
    column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
      column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("trip_id", "vesseltype_code", "vesseltype_label"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("trip_id", "vesseltype_code", "vesseltype_label"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code", "vesseltype_label")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("landing_id", "trip_id", "landing_weight", "weightcategory_code"),
    column_type = c("character", "character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("landing_id", "trip_id", "landing_weight", "weightcategory_code"),
      column_type = c("character", "character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe4 <- dataframe4[, c("landing_id", "trip_id", "landing_weight", "weightcategory_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_type,
    type = "character",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      length = 2L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_weight,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_weight,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_ratio,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_ratio,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = landing_type_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = landing_type_baitboat,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = sample_type_code_landing_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = sample_type_code_landing_baitboat,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_baitboat,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_baitboat,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), 0, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE))) %>%
    dplyr::ungroup()
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight = ifelse(is.na(sample_totalweight) | sample_totalweight == 0, weight_calculation, sample_totalweight)) %>%
    dplyr::ungroup()
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)), .groups = "drop")
  # Calculation fresh landing baitboat (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe4 <- dataframe4 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::filter(weightcategory_code %in% landing_type_baitboat) %>%
    dplyr::summarise(sum_landing_weight_baitboat = ifelse(all(is.na(landing_weight)), 0, sum(landing_weight, na.rm = TRUE)), .groups = "drop")
  # Merge
  dataframe1$logical <- TRUE
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(trip_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe4, by = dplyr::join_by(trip_id))
  # Case of NA weightedweight or sum_landing_weight_baitboat
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
                  weightedweight_bis = dplyr::coalesce(weightedweight, 0),
                  sum_landing_weight_baitboat_bis = dplyr::coalesce(sum_landing_weight_baitboat, 0))
  # Check
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weight > threshold_weight, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weightedweight_bis < dataframe1$weight & !((dataframe1$weightedweight_bis / dataframe1$weight) >= threshold_ratio), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & dataframe1$sampletype_code %in% sample_type_code_landing_baitboat & abs(dataframe1$weightedweight_bis - dataframe1$sum_landing_weight_baitboat_bis) > threshold_baitboat, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & !(dataframe1$sampletype_code %in% sample_type_code_landing_baitboat) & abs(dataframe1$weightedweight_bis - dataframe1$weight) > threshold_baitboat, "logical"] <- FALSE
  # Case NA vesseltype_code sampletype_code
  dataframe1[is.na(dataframe1$vesseltype_code), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & is.na(dataframe1$sampletype_code), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_calculation, weight, vesseltype_code, weightedweight_bis, sum_landing_weight_baitboat_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_totalweight, sampletype_code, weightedweight, vesseltype_label, sum_landing_weight_baitboat, .after = logical) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with weighted weight", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' @name check_weight_sample_inspector
#' @title Gives the inconsistencies between the sample weight (m10 and p10) and the global sample weight
#' @description The purpose of the check_weight_sample_inspector  function is to provide a table of data that contains an inconsistency between the sample weight (m10 and p10) and the global weight
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weight_sample_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, and 2 are ok,
#' #Sample 3 has total weight and smalls weight,
#' #Sample 4 has not total weight nor smalls weight nor bigs weight
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4"),
#'                          sample_smallsweight = c(10, NA, 12, NA),
#'                          sample_bigsweight = c(50, NA, NA, 0),
#'                          sample_totalweight = c(NA, 9, 5, 0))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE, FALSE, FALSE), sample_totalweight = c(NA, 9, 5, 0), sample_smallsweight = c(10, NA, 12, NA), sample_bigsweight = c(50, NA, NA, 0)), row.names = c(NA, -4L), class = "data.frame"))
#' check_weight_sample_inspector(dataframe1, output = "report")
#' @export
check_weight_sample_inspector <- function(dataframe1,
                                          output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  weight_calculation <- NULL
  sample_totalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), NaN, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE))) %>%
    dplyr::ungroup()
  # Check
  comparison_weight_calculation <- codama::vector_comparison(
    first_vector = dataframe1$weight_calculation,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  comparison_totalweight <- codama::vector_comparison(
    first_vector = dataframe1$sample_totalweight,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  # Checks that a weight has been indicated
  dataframe1$logical <- !(comparison_weight_calculation$logical & comparison_totalweight$logical) & !(is.na(dataframe1$weight_calculation) & is.na(dataframe1$sample_totalweight))
  # Checks that a weight is indicated either for the total weight or for one of the two weight categories
  dataframe1$logical[!is.na(dataframe1$weight_calculation) & dataframe1$weight_calculation > 0 & !is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight > 0] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(weight_calculation))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_totalweight, sample_smallsweight, sample_bigsweight, .after = logical) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample weight (m10 and p10) inconsistency with the global weight", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_activity_sample_inspector
#' @title Gives inconsistencies between the sample and the activity in terms of presence
#' @description The purpose of the check_activity_sample_inspector function is to provide a table of data that contains an inconsistency between the sample and the existence of the activity
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_activity_sample_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_activity_sample_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 is ok,
#' #Sample 2 is not linked to an activity
#' dataframe1 <- data.frame(sample_id = c("1", "2"))
#' dataframe2 <- data.frame(sample_id = c("1"),
#'                          activity_id = c("1"))
#' @expect equal(., structure(list(sample_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' check_activity_sample_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_activity_sample_inspector <- function(dataframe1,
                                            dataframe2,
                                            output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "activity_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  dataframe2 <- dataframe2[!is.na(dataframe2$activity_id), ]
  # Check
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$sample_id,
    second_vector = dataframe2$sample_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample not linked to any activity", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_ldlf_inspector
#' @title Gives the inconsistencies between the sample measurement types and species or weight values
#' @description The purpose of the check_ldlf_inspector  function is to provide a table of data that contains an inconsistency between the sample measurement types and species or weight values
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values:  c("SKJ", "LTA", "FRI"). Vector of the species not to be associated with a type of measure.
#' @param size_measure_type_species {\link[base]{character}} expected. Default values:  c("PD1"). Vector of type of measure not to be associated with species
#' @param size_measure_type_big {\link[base]{character}} expected. Default values:  c("PD1"). Type of measure that must have a total weight or a big fish weight
#' @param size_measure_type_small {\link[base]{character}} expected. Default values: c("FL"). Type of measure that must have a total weight or a small fish weight
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample species 1, 3 and 4 are ok,
#' #Sample species 2 has a species that is not compatible with the type of measure,
#' #Sample species 5 has no smalls weight nor total weight,
#' #Sample species 6 has no bigs weight nor total weight
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6"),
#'                          species_fao_code = c("SKJ", "SKJ", "JOS", "SKJ", "SKJ", "JOS"),
#'                          sizemeasuretype_code = c("FL", "PD1", "PD1", "FL", "FL", "PD1"),
#'                          sample_id = c("1", "1", "1", "2", "3", "4"))
#' dataframe2 <- data.frame(sample_id = c("1", "2", "3", "4"),
#'                          sample_smallsweight = c(5, 0, 0, 7),
#'                          sample_bigsweight = c(12, NA, 6, 0),
#'                          sample_totalweight = c(0, 23, NA, NA))
#' @expect equal(., structure(list(samplespecies_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), sizemeasuretype_code = c("FL", "PD1", "PD1", "FL", "FL", "PD1"), species_fao_code = c("SKJ", "SKJ", "JOS", "SKJ", "SKJ", "JOS"), sample_bigsweight = c(12, 12, 12, NA, 6, 0), sample_smallsweight = c(5, 5, 5, 0, 0, 7), sample_totalweight = c(0, 0, 0, 23, NA, NA)), row.names = c(NA, 6L), class = "data.frame"))
#' check_ldlf_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_ldlf_inspector <- function(dataframe1,
                                 dataframe2,
                                 output,
                                 species = c("SKJ", "LTA", "FRI"),
                                 size_measure_type_species = c("PD1"),
                                 size_measure_type_big = c("PD1"),
                                 size_measure_type_small = c("FL")) {
  # 0 - Global variables assignement ----
  logical_species <- NULL
  logical_bigsweight <- NULL
  logical_smallsweight <- NULL
  sample_id <- NULL
  sizemeasuretype_code <- NULL
  species_fao_code <- NULL
  sample_bigsweight <- NULL
  sample_smallsweight <- NULL
  sample_totalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type_species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type_small,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_small,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Check species and measuretype
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  comparison_size_measure_type_species <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_species <- !(comparison_species$logical & comparison_size_measure_type_species$logical)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  # Check bigs weight and measuretype
  comparison_size_measure_type_big <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_big,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_bigsweight <- !(comparison_size_measure_type_big$logical & ((is.na(dataframe1$sample_bigsweight) | (!is.na(dataframe1$sample_bigsweight) & dataframe1$sample_bigsweight == 0)) & (is.na(dataframe1$sample_totalweight) | (!is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight == 0))))
  # Check smalls weight and measuretype
  comparison_size_measure_type_small <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_small,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_smallsweight <- !(comparison_size_measure_type_small$logical & ((is.na(dataframe1$sample_smallsweight) | (!is.na(dataframe1$sample_smallsweight) & dataframe1$sample_smallsweight == 0)) & (is.na(dataframe1$sample_totalweight) | (!is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight == 0))))
  # Check
  dataframe1$logical <- dataframe1$logical_species & dataframe1$logical_bigsweight & dataframe1$logical_smallsweight
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(logical_species, logical_bigsweight, logical_smallsweight, sample_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sizemeasuretype_code, species_fao_code, sample_bigsweight, sample_smallsweight, sample_totalweight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample with inconsistency between the sample measurement types and species or weight values", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_category_species_forbidden_well_inspector
#' @title Gives the inconsistencies between the weight categories and the species in the well
#' @description The purpose of the check_category_species_forbidden_well_inspector function is to provide a table of data that contains an inconsistency between the weight categories and the species in the well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("SKJ"). Vector of species that must not have certain weight categories in well (weight_category)
#' @param weight_category {\link[base]{character}} expected. Default values: c("W-9"). Vector of weight category codes that must not have certain species in well (species)
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  weightcategory_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample well 1 and 2 are ok,
#' #Sample well 3 is associated with the species concerned (species) and the weight category
#' #              concerned (weight_category)
#' dataframe1 <- data.frame(wellactivityspecies_id = c("1", "2", "3"),
#'                          weightcategory_code = c("W-1", "W-9", "W-9"),
#'                          species_fao_code = c("SKJ", "ALB", "SKJ"))
#' @expect equal(., structure(list(wellactivityspecies_id = c("1", "2", "3"), species_fao_code  = c("SKJ", "ALB", "SKJ"), weightcategory_code = c("W-1", "W-9", "W-9"), logical = c(TRUE, TRUE, FALSE)), row.names = c(NA, -3L), class = "data.frame"))
#' check_category_species_forbidden_well_inspector(dataframe1, output = "report")
#' @export
check_category_species_forbidden_well_inspector <- function(dataframe1,
                                                            output,
                                                            species = c("SKJ"),
                                                            weight_category = c("W-9")) {
  # 0 - Global variables assignement ----
  weightcategory_code <- NULL
  species_fao_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "species_fao_code", "weightcategory_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "species_fao_code", "weightcategory_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("wellactivityspecies_id", "species_fao_code", "weightcategory_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$wellactivityspecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Check weight_category and species
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(logical = ifelse(weightcategory_code %in% weight_category & species_fao_code %in% species, FALSE, TRUE))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$wellactivityspecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$wellactivityspecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " well activity with inconsistencies between the weight categories and the species in the well", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_distribution_inspector
#' @title Gives the inconsistencies between the weights of small and big sample fish and the sum of the small and big weights in the associated well
#' @description The purpose of the check_distribution_inspector  function is to provide a table of data that contains an inconsistency between the small and large sample weights and the sum of the small and big weights of the associated well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species_category_small_big {\link[base]{character}} expected. Default values: c("ALB", "YFT", "BET", "SKJ"). List of the inventory of species (FAO code) used to calculate weight category small and big in well
#' @param species_category_unknown {\link[base]{character}} expected. Default values: c("SKJ"). Vector of species categorized as small if weight category information is missing
#' @param weight_category_small {\link[base]{character}} expected. Default values: c("W-1"). Vector of small weight category codes
#' @param weight_category_big {\link[base]{character}} expected. Default values: c("W-2"). Vector of big weight category codes
#' @param weight_category_unknown {\link[base]{character}} expected. Default values: c("W-9"). Vector of unknown weight category codes
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  well_id}}
#'  \item{\code{  well_label}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  well_id}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  weightcategory_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  wellactivityspecies_weight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 has not small weight in well,
#' #Sample 4 has not bigs weight in sample,
#' #Sample 5 has different bigs weight,
#' #Sample 6 and 7 has different small weight
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          sample_well = c("well_1", "well_2", "well_3", "well_4", "well_5",
#'                                          "well_6","well_7"),
#'                          trip_id = c("1", "1", "1", "1", "1", "1", "1"),
#'                          sample_smallsweight = c(6, 25, 14, 0, NA, 10, 8),
#'                          sample_bigsweight = c(12, 0, 9, NA, 6, 0, 0))
#' dataframe2 <- data.frame(well_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          well_label = c("well_1", "well_2", "well_3", "well_4", "well_5", "well_6",
#'                                         "well_7"),
#'                          trip_id = c("1", "1", "1", "1", "1", "1", "1"))
#' dataframe3 <- data.frame(wellactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          well_id = c("1", "1", "2", "3", "4", "5", "6", "7"))
#' dataframe4 <- data.frame(wellactivityspecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
#'                                                     "10", "11", "12", "13", "14"),
#'                          wellactivity_id = c("1", "1", "1", "2", "3", "4", "4", "5", "6", "7",
#'                                              "7", "7", "8", "8"),
#'                          weightcategory_code = c("W-1", "W-9", "W-2", "W-2", "W-1", "W-2", "W-9",
#'                                                  "W-2", "W-2", "W-1", "W-9", "W-9", "W-1", "W-1"),
#'                          species_fao_code = c("BET", "SKJ", "SKJ", "ALB", "SKJ", "BET", "BET",
#'                                               "ALB", "SKJ", "SKJ", "SKJ", "BET", "ALB", "JOS"),
#'                          wellactivityspecies_weight = c(4, 2, 7, 5, 25, 9, 14, 5, 17, 10, 5, 2, 7,
#'                                                         1))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), sample_smallsweight = c(6, 25, 14, 0, NA, 10, 8), sample_bigsweight = c(12, 0, 9, NA, 6, 0, 0), sample_well = c("well_1", "well_2", "well_3", "well_4", "well_5", "well_6", "well_7"), weight_sum_small_filter = c(6, 25, NaN, NaN, NaN, 15, 7), weight_sum_big_filter = c(12, NaN, 9, 5, 17, NaN, NaN), weight_sum_small = c(6, 25, 14, NaN, NaN, 17, 8), weight_sum_big = c(12, NaN, 9, 5, 17, NaN, NaN)), row.names = c(NA, -7L), class = "data.frame"))
#' check_distribution_inspector(dataframe1, dataframe2, dataframe3, dataframe4, output = "report")
#' @export
check_distribution_inspector <- function(dataframe1,
                                         dataframe2,
                                         dataframe3,
                                         dataframe4,
                                         output,
                                         species_category_small_big = c("ALB", "YFT", "BET", "SKJ"),
                                         species_category_unknown = c("SKJ"),
                                         weight_category_small = c("W-1"),
                                         weight_category_big = c("W-2"),
                                         weight_category_unknown = c("W-9")) {
  # 0 - Global variables assignement ----
  well_id <- NULL
  trip_id <- NULL
  well_label <- NULL
  weightcategory_code <- NULL
  wellactivityspecies_weight <- NULL
  species_fao_code <- NULL
  weight_small <- NULL
  weight_small_unknown <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  weight_big_filter <- NULL
  weight_small_filter <- NULL
  weight_big_filter_species <- NULL
  weight_small_filter_species <- NULL
  weight_sum_small <- NULL
  weight_sum_big <- NULL
  weight_sum_small_filter <- NULL
  weight_sum_big_filter <- NULL
  sample_smallsweight_bis <- NULL
  sample_bigsweight_bis <- NULL
  weight_small_total_bis <- NULL
  weight_big_bis <- NULL
  sample_well <- NULL
  wellactivity_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
    column_type = c("character", "character", "character", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
      column_type = c("character", "character", "character", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("well_id", "well_label", "trip_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("well_id", "well_label", "trip_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("well_id", "well_label", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("wellactivity_id", "well_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("wellactivity_id", "well_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("wellactivity_id", "well_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight"),
    column_type = c("character", "character", "character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight"),
      column_type = c("character", "character", "character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe4 <- dataframe4[, c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_category_small_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_category_small_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_category_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_category_unknown,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_small,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_small,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_unknown,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe2 <- dplyr::left_join(dataframe2, dataframe3, by = dplyr::join_by(well_id))
  dataframe2 <- dplyr::left_join(dataframe2, dataframe4, by = dplyr::join_by(wellactivity_id))
  # Calculation small weight and big weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  # weight_sum_small and weight_sum_big are calculated to give information to the user but are not used in the control
  dataframe2_sum_weight <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_label) %>%
    dplyr::mutate(weight_small_filter = ifelse((weightcategory_code %in% weight_category_small) | (weightcategory_code %in% weight_category_unknown), wellactivityspecies_weight, NA),
                  weight_small_filter_species = ifelse((weightcategory_code %in% weight_category_small & species_fao_code %in% species_category_small_big) | (weightcategory_code %in% weight_category_unknown & species_fao_code %in% species_category_unknown), wellactivityspecies_weight, NA),
                  weight_big_filter = ifelse(weightcategory_code %in% weight_category_big, wellactivityspecies_weight, NA),
                  weight_big_filter_species = ifelse(weightcategory_code %in% weight_category_big & species_fao_code %in% species_category_small_big, wellactivityspecies_weight, NA)) %>%
    dplyr::summarise(weight_sum_small_filter = ifelse(all(is.na(weight_small_filter_species)), NaN, sum(weight_small_filter_species, na.rm = TRUE)),
                     weight_sum_small = ifelse(all(is.na(weight_small_filter)), NaN, sum(weight_small_filter, na.rm = TRUE)),
                     weight_sum_big_filter = ifelse(all(is.na(weight_big_filter_species)), NaN, sum(weight_big_filter_species, na.rm = TRUE)),
                     weight_sum_big = ifelse(all(is.na(weight_big_filter)), NaN, sum(weight_big_filter, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::select(-well_id)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2_sum_weight, by = dplyr::join_by(trip_id == trip_id, sample_well == well_label))
  # Calculation small weight total (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  # Case of NA
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
      sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
      weight_big_bis = dplyr::coalesce(weight_sum_big_filter, 0),
      weight_small_total_bis = dplyr::coalesce(weight_sum_small_filter, 0)
    )
  # Check small weight
  comparison_smallsweight <- codama::vector_comparison(
    first_vector = dataframe1$weight_small_total_bis,
    second_vector = dataframe1$sample_smallsweight_bis,
    comparison_type = "equal",
    output = "report"
  )
  # Check smalls weight
  comparison_bigsweight <- codama::vector_comparison(
    first_vector = dataframe1$weight_big_bis,
    second_vector = dataframe1$sample_bigsweight_bis,
    comparison_type = "equal",
    output = "report"
  )
  # Check
  dataframe1$logical <- comparison_smallsweight$logical & comparison_bigsweight$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_small_unknown, weight_small, sample_smallsweight_bis, sample_bigsweight_bis, weight_small_total_bis, weight_big_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_well, weight_sum_small_filter, weight_sum_big_filter, weight_sum_small, weight_sum_big, .after = logical) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample with inconsistencies in distribution with the well", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_sample_harbour_inspector
#' @title Gives inconsistencies between the presence of a sample and the absence of a harbour of landing
#' @description The purpose of the check_sample_harbour_inspector function is to provide a table of data that contains an inconsistency between the presence of a sample and the absence of a harbour of landing
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_harbour_inspector function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_harbour_inspector function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_id_landing}}
#'  \item{\code{  harbour_label_landing}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 is ok,
#' #Sample 2 has no landing harbour
#' dataframe1 <- data.frame(sample_id = c("1", "2"),
#'                          trip_id = c("1", "2"))
#' dataframe2 <- data.frame(trip_id = c("1"),
#'                          harbour_id_landing = c("1"),
#'                          harbour_label_landing = c("harbour_1"))
#' @expect equal(., structure(list(sample_id = c("1", "2"), logical = c(TRUE, FALSE), harbour_label_landing = c("harbour_1", NA)), row.names = 1:2, class = "data.frame"))
#' check_sample_harbour_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sample_harbour_inspector <- function(dataframe1,
                                           dataframe2,
                                           output) {
  # 0 - Global variables assignement ----
  harbour_label_landing <- NULL
  harbour_id_landing <- NULL
  trip_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "trip_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "trip_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "harbour_id_landing", "harbour_label_landing"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "harbour_id_landing", "harbour_label_landing"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "harbour_id_landing", "harbour_label_landing")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- TRUE
  } else {
    dataframe1$logical <- logical()
  }
  # Check if missing harbour
  dataframe1[is.na(dataframe1$harbour_id_landing), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, harbour_label_landing, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(harbour_id_landing, trip_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample without harbour of landing", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_anapo_inspector
#' @title Gives the inconsistencies activity position and VMS position
#' @description The purpose of the check_anapo_inspector function is to provide a table of data that contains an inconsistency between activity position and VMS position
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param harbour_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position harbour
#' @param vms_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position VMS
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold_number_vms {\link[base]{numeric}} expected. Default values: 20. Minimum number of VMS positions required per day.
#' @param threshold_geographical {\link[base]{numeric}} expected. Default values: 10. Maximum valid distance threshold (Nautical miles) between position and nearest VMS point.
#' @param threshold_time {\link[base]{numeric}} expected. Default values: 7200000. Maximum valid distance threshold (milliseconds) between position and VMS point.
#' @param threshold_score {\link[base]{numeric}} expected. Default values: 0.5. Minimum valid score between position and VMS point.
#' @param buffer_harbour {\link[base]{numeric}} expected. Default values: 11100. Buffer to be used for harbour, in meter
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  harbour_position_departure}}
#'  \item{\code{  harbour_position_landing}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  vms_date}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  vessel_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Activity 1, 2 and 3 are ok,
#' #Activity 4 has a number of VMS below the threshold (threshold_number_vms),
#' #Activity 5 has no position,
#' #Activity 6 has has a geographical distance above the threshold (threshold_geographical) and
#' #           a score below the threshold (threshold_score)
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12",
#'                                                    "2020/01/13", "2020/01/12", "2020/01/12")),
#'                          activity_time = c("05:26:01", "10:41:15", "16:41:15", "03:12:34",
#'                                            "05:56:12", "23:26:47"),
#'                          activity_position = c("POINT (1 1)", "POINT (0 0)", "POINT (3 0)",
#'                                                "POINT (4 4)", NA, "POINT (3 0.6)"),
#'                          trip_id = c("1", "2", "2", "2", "2", "2"))
#' dataframe2 <- data.frame(trip_id = c("1", "2"),
#'                          vessel_code = c("1", "1"),
#'                          harbour_position_departure = c("POINT (1 1.1)", "POINT (3 3)"),
#'                          harbour_position_landing = c("POINT (3 3)", "POINT (3 3)"))
#' dataframe3 <- data.frame(vms_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12")),
#'                          vms_time = c("15:26:01", "10:55:15", "22:32:17"),
#'                          vms_position = c("POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"),
#'                          vessel_code = c("1", "1", "1"))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), nb_vms = c(1L, 2L, 2L, NA, 2L, 2L), min_distance = structure(c(NA, 6.004055139173, 18.012165417519, 230.106883933216, NA, 18.012165417519), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), max_score = c(NA, NA, 2.17959673670807, 0, NA, 0.209441144555651)), row.names = c(NA, 6L), class = "data.frame"), structure(list(activity_id = c("3", "3", "4", "4", "6", "6", "1", "2", "2", "5", "5"), activity_date = structure(c(18273, 18273, 18274, 18274, 18273, 18273, 18262, 18273, 18273, 18273, 18273), class = "Date"), activity_time = c("16:41:15", "16:41:15", "03:12:34", "03:12:34", "23:26:47", "23:26:47", "05:26:01", "10:41:15", "10:41:15", "05:56:12", "05:56:12"), activity_position = c("POINT (3 0)", "POINT (3 0)", "POINT (4 4)", "POINT (4 4)", "POINT (3 0.6)", "POINT (3 0.6)", "POINT (1 1)", "POINT (0 0)", "POINT (0 0)", NA, NA), vms_date = structure(c(18273, 18273, 18273, 18273, 18273, 18273, 18262, 18273, 18273, 18273, 18273), class = "Date"), vms_time = c("10:55:15", "22:32:17", "10:55:15", "22:32:17", "10:55:15", "22:32:17", "15:26:01", "10:55:15", "22:32:17", "10:55:15", "22:32:17"), vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)"), distance = structure(c(180.221602566745, 18.012165417519, 335.278629168604, 230.106883933216, 182.602328607533, 18.012165417519, NA, 6.004055139173, 181.019203021658, NA, NA), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(20760000, -21062000, -27761000, -69583000, 45092000, 3270000, NA, NA, NA, NA, NA), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(0, 2.17959673670807, 0, 0, 0, 0.209441144555651, NA, NA, NA, NA, NA), vms_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, -11L), class = "data.frame")))
#' check_anapo_inspector(dataframe1,dataframe2, dataframe3, output = "report",threshold_number_vms = 1)
#' @export
check_anapo_inspector <- function(dataframe1,
                                  dataframe2,
                                  dataframe3,
                                  activity_crs = 4326,
                                  harbour_crs = 4326,
                                  vms_crs = 4326,
                                  output,
                                  threshold_number_vms = 20,
                                  threshold_geographical = 10,
                                  threshold_time = 7200000,
                                  threshold_score = 0.5,
                                  buffer_harbour = 11100) {
  # 0 - Global variables assignement ----
  . <- NULL
  trip_id <- NULL
  harbour_position_departure <- NULL
  harbour_buffer_departure <- NULL
  harbour_position_landing <- NULL
  harbour_buffer_landing <- NULL
  logical_harbourdeparture <- NULL
  logical_harbourlanding <- NULL
  vms_date <- NULL
  vessel_code <- NULL
  nb_vms <- NULL
  activity_position_geometry <- NULL
  vms_position_geometry <- NULL
  activity_id <- NULL
  distance <- NULL
  min_distance <- NULL
  activity_time_bis <- NULL
  vms_time <- NULL
  activity_date_time <- NULL
  vms_date_time <- NULL
  duration <- NULL
  score <- NULL
  nb_vms_bis <- NULL
  activity_date <- NULL
  activity_time <- NULL
  activity_position <- NULL
  date_group <- NULL
  vms_position <- NULL
  NM <- NULL
  ms <- NULL
  X <- NULL
  Y <- NULL
  logical_bounding <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id"),
    column_type = c("character", "Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id"),
      column_type = c("character", "Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("vms_date", "vms_time", "vms_position", "vessel_code"),
    column_type = c("Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("vms_date", "vms_time", "vms_position", "vessel_code"),
      column_type = c("Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("vms_date", "vms_time", "vms_position", "vessel_code")]
  }
  if (!codama::r_type_checking(
    r_object = activity_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = harbour_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = harbour_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vms_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vms_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_number_vms,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_number_vms,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_geographical,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_geographical,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_score,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_score,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = buffer_harbour,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = buffer_harbour,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Indicates activity whether in harbour
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Indicates whether in land harbour
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- FALSE
  } else {
    dataframe1$logical <- logical()
  }
  # Formats spatial data activity
  dataframe_activity_geo <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(wkt = "activity_position", crs = activity_crs, remove = FALSE) %>%
    sf::st_transform(activity_position, crs = 4326)
  sf::st_geometry(dataframe_activity_geo) <- "activity_position_geometry"
  # Retrieves point coordinates
  if (nrow(dataframe_activity_geo) > 0) {
    dataframe_activity_geo <- dataframe_activity_geo %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
  } else {
    dataframe_activity_geo$X <- numeric()
    dataframe_activity_geo$Y <- numeric()
  }
  # Checks whether the point is within the bounds of CRS 4326
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::mutate(logical_bounding = (X >= -180 & X <= 180 & Y >= -90 & Y <= 90))
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::filter(logical_bounding)
  # If harbour departure position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_departure))) {
    # Formats spatial data harbourdeparture, add buffer in meter
    data_geo_harbourdeparture <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_departure)) %>%
      dplyr::select(harbour_position_departure) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_departure = harbour_position_departure) %>%
      sf::st_as_sf(wkt = "harbour_buffer_departure", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_departure, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourdeparture
    dataframe_activity_geo_harbourdeparture <- dataframe_activity_geo %>% dplyr::filter(!is.na(harbour_position_departure))
    intersect_harbourdeparture <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) lengths(sf::st_intersects(dataframe_activity_geo_harbourdeparture[dataframe_activity_geo_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x], 1], data_geo_harbourdeparture[x, 1])))
    intersect_harbourdeparture_index <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) which(dataframe_activity_geo_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x]))
    dataframe_activity_geo_harbourdeparture$nb_port_intersect <- 0
    dataframe_activity_geo_harbourdeparture$nb_port_intersect[unlist(intersect_harbourdeparture_index)] <- unlist(intersect_harbourdeparture)
    dataframe_activity_geo_harbourdeparture$logical_harbourdeparture <- dataframe_activity_geo_harbourdeparture$nb_port_intersect != 0
    # Logical harbourdeparture
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(dataframe_activity_geo_harbourdeparture)[, c("activity_id", "logical_harbourdeparture")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourdeparture[is.na(dataframe1$logical_harbourdeparture)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourdeparture <- FALSE
    } else {
      dataframe1$logical_harbourdeparture <- logical()
    }
  }
  # If harbour landing position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_landing))) {
    # Formats spatial data harbourlanding, add buffer in meter
    data_geo_harbourlanding <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_landing)) %>%
      dplyr::select(harbour_position_landing) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_landing = harbour_position_landing) %>%
      sf::st_as_sf(wkt = "harbour_buffer_landing", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_landing, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourlanding
    dataframe_activity_geo_harbourlanding <- dataframe_activity_geo %>% dplyr::filter(!is.na(harbour_position_landing))
    intersect_harbourlanding <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) lengths(sf::st_intersects(dataframe_activity_geo_harbourlanding[dataframe_activity_geo_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x], 1], data_geo_harbourlanding[x, 1])))
    intersect_harbourlanding_index <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) which(dataframe_activity_geo_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x]))
    dataframe_activity_geo_harbourlanding$nb_port_intersect <- 0
    dataframe_activity_geo_harbourlanding$nb_port_intersect[unlist(intersect_harbourlanding_index)] <- unlist(intersect_harbourlanding)
    dataframe_activity_geo_harbourlanding$logical_harbourlanding <- dataframe_activity_geo_harbourlanding$nb_port_intersect != 0
    # Logical in harbourlanding
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(dataframe_activity_geo_harbourlanding)[, c("activity_id", "logical_harbourlanding")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourlanding[is.na(dataframe1$logical_harbourlanding)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourlanding <- FALSE
    } else {
      dataframe1$logical_harbourlanding <- logical()
    }
  }
  # Logical in harbour
  dataframe1$logical <- dataframe1$logical | dataframe1$logical_harbourdeparture | dataframe1$logical_harbourlanding
  # Remove VMS without position
  dataframe3 <- dataframe3 %>% dplyr::filter(!is.na(vms_position))
  # Calculation number vms
  dataframe3_nb_vms <- dataframe3 %>%
    dplyr::group_by(vms_date, vessel_code) %>%
    dplyr::summarise(nb_vms = dplyr::n(), .groups = "drop")
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3_nb_vms, by = dplyr::join_by(activity_date == vms_date, vessel_code == vessel_code))
  # Case of NA nb_vms
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      nb_vms_bis = dplyr::coalesce(nb_vms, 0),
    )
  # Retrieves VMS positions for the previous, current and next day
  dataframe3$date_group <- dataframe3$vms_date
  dataframe3_prior <- dataframe3 %>% dplyr::mutate(date_group = vms_date - 1)
  dataframe3_post <- dataframe3 %>% dplyr::mutate(date_group = vms_date + 1)
  dataframe3 <- dplyr::bind_rows(dataframe3, dataframe3_prior, dataframe3_post) %>%
    dplyr::group_by(date_group, vms_position) %>%
    dplyr::distinct()
  dataframe3 <- dplyr::inner_join(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position", "logical")], dataframe3, by = dplyr::join_by(activity_date == date_group, vessel_code == vessel_code), relationship = "many-to-many")
  # Formats spatial data vms
  dataframe_vms_geo <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    dplyr::select(vms_position) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(wkt = "vms_position", crs = vms_crs, remove = FALSE) %>%
    sf::st_transform(vms_position, crs = 4326)
  sf::st_geometry(dataframe_vms_geo) <- "vms_position_geometry"
  # Select unique pair vms/activity
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::select(activity_position, activity_position_geometry) %>%
    dplyr::distinct()
  pair_position <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    dplyr::select(vms_position, activity_position) %>%
    dplyr::distinct() %>%
    dplyr::mutate(pair_position = paste0(activity_position, "_", vms_position))
  pair_position <- dplyr::left_join(pair_position, dataframe_vms_geo, by = dplyr::join_by(vms_position))
  pair_position <- dplyr::left_join(pair_position, dataframe_activity_geo, by = dplyr::join_by(activity_position))
  # Calculation of the minimum distance between the activity and the nearest day's VMS in nautical mile
  # Define nautical miles
  units::install_unit("NM", "1852 m", "Nautical mile")
  threshold_geographical <- units::set_units(threshold_geographical, NM)
  pair_position <- pair_position %>%
    dplyr::mutate(distance = sf::st_distance(x = activity_position_geometry, y = vms_position_geometry, by_element = TRUE))
  if (nrow(pair_position) > 0) {
    units(pair_position$distance) <- units::make_units(NM)
  }else {
    units(pair_position$distance) <- units::drop_units(pair_position$distance)
    units(pair_position$distance) <- units::make_units(NM)
  }
  # Remove formats spatial data
  pair_position <- pair_position %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-c(activity_position_geometry, vms_position_geometry))
  dataframe3 <- dplyr::left_join(dataframe3, pair_position[, c("distance", "vms_position", "activity_position")], by = dplyr::join_by(vms_position, activity_position))
  rm(pair_position)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::mutate(min_distance = ifelse(length(distance) > 0, min(distance), Inf)) %>%
    dplyr::ungroup()
  units(dataframe3$min_distance) <- units::make_units(NM)
  dataframe_calcul_min <- dataframe3 %>%
    dplyr::select(activity_id, min_distance) %>%
    dplyr::distinct()
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_calcul_min, by = dplyr::join_by(activity_id))
  # Check if distance between activity and nearest VMS point below threshold
  if (nrow(dataframe1) > 0) {
    dataframe1[!is.na(dataframe1$min_distance) & dataframe1$min_distance < threshold_geographical, "logical"] <- TRUE
  } else {
    dataframe1[!is.na(dataframe1$min_distance) & units::drop_units(dataframe1$min_distance) < units::drop_units(threshold_geographical), "logical"] <- logical()
  }
  dataframe_calcul <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    subset(select = -c(logical)) %>%
    dplyr::filter(units::drop_units(min_distance) >= units::drop_units(threshold_geographical))
  # Gives a temporary hour for activities that are missing an hour
  dataframe_calcul$activity_time_bis <- dataframe_calcul$activity_time
  dataframe_calcul[is.na(dataframe_calcul$activity_time), "activity_time_bis"] <- "00:00:00"
  # Calculates time between activity and VMS point in milliseconds
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(activity_date_time = as.POSIXct(paste(vms_date, activity_time_bis)), vms_date_time = as.POSIXct(paste(vms_date, vms_time)))
  units::install_unit("ms", "1000 secs", "Milliseconds")
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(duration = units::set_units(as.numeric(difftime(activity_date_time, vms_date_time, units = "secs") * 1000), ms))
  # Gives a duration for activities that are missing an hour
  dataframe_calcul[is.na(dataframe_calcul$activity_time), "duration"] <- units::set_units(1, ms)
  # Score calculation
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(score = (2^(units::drop_units(-distance / threshold_geographical))) * (2^(-units::drop_units(duration) / threshold_time)))
  dataframe_calcul[units::drop_units(dataframe_calcul$distance) > units::drop_units(threshold_geographical * 2), "score"] <- 0
  dataframe_calcul[as.numeric(dataframe_calcul$duration) > threshold_time * 2, "score"] <- 0
  dataframe_score_max <- dataframe_calcul %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(max_score = ifelse(length(score) > 0, max(score), -Inf))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_score_max, by = dplyr::join_by(activity_id))
  # Check the maximum score between activity and VMS
  dataframe1[!is.na(dataframe1$max_score) & dataframe1$max_score >= threshold_score, "logical"] <- TRUE
  # Check if the number of vms for the day exceeds the threshold
  dataframe1[dataframe1$nb_vms_bis < threshold_number_vms, "logical"] <- FALSE
  # Recovers all activity positions for the detailed table
  # Data with calcul VMS
  dataframe_detail <- dplyr::bind_rows(dataframe_calcul, dplyr::anti_join(subset(dataframe3, select = -c(logical)), dataframe_calcul, by = c("activity_id", "activity_date", "vms_date", "vessel_code", "vms_time", "vms_position", "activity_time", "activity_position")))
  dataframe_detail <- dplyr::bind_rows(dataframe_detail, dplyr::anti_join(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position")], dataframe3, by = c("activity_date" = "activity_date", "vessel_code" = "vessel_code")))
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, harbour_position_departure, harbour_position_landing, logical_harbourdeparture, logical_harbourlanding, nb_vms_bis, activity_date, vessel_code, activity_time, activity_position))
  dataframe_detail <- subset(dataframe_detail, select = -c(vessel_code, min_distance, activity_time_bis, activity_date_time, vms_date_time))
  dataframe_detail <- dataframe_detail %>%
    dplyr::mutate(vms_crs = vms_crs, activity_crs = activity_crs) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with missing VMS", collapse = ", ")))
  }
  if (output == "report") {
    return(list(dataframe1, dataframe_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

#' @name check_anapo_activity_consistent_inspector
#' @title Gives the inconsistencies between the VMS and the presence of activity
#' @description The purpose of the check_anapo_activity_consistent_inspector function is to provide a table of data that contains an inconsistency between the VMS and the presence of activit
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_activity_consistent_inspector function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_activity_consistent_inspector function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("1", "2", "5", "6", "10"). List of vessel type code to be inspected
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  vms_id}}
#'  \item{\code{  vms_date}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  vms_codevessel}}
#'  \item{\code{  vessel_type}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  vessel_code}}
#' }
#' @doctest
#' #VMS 1, 3, 4 and 5 are ok,
#' #VMS 2 has not linked to an activity
#' dataframe1 <- data.frame(vms_id = c("1", "2", "3", "4", "5"),
#'                          vms_date = as.Date(c("2020/01/01", "2020/01/02", "2020/02/02",
#'                                               "2020/02/02", "2020/02/03")),
#'                          vessel_code = c("1", "1", "2", "3", "4"),
#'                          vms_codevessel = c("vessel_1", "vessel_1", "vessel_2", "vessel_2",
#'                                             "vessel_4"),
#'                          vessel_type = c("1", "1", "1", "1", "3"))
#' dataframe2 <- data.frame(activity_id = c("1", "2"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/02/02")),
#'                          vessel_code = c("1", "2"))
#' @expect equal(., structure(list(vms_id = c("1", "2", "3", "4", "5"), vessel_code = c("1", "1", "2", "3", "4"), vms_date = structure(c(18262, 18263, 18294, 18294, 18295), class = "Date"), vms_codevessel = c("vessel_1", "vessel_1", "vessel_2", "vessel_2", "vessel_4"), vessel_type = c("1", "1", "1", "1", "3"), logical = c(TRUE, FALSE, TRUE, TRUE, TRUE), nb_activity = c(1, 0, 1, 0, 0)), row.names = c(NA, -5L), class = "data.frame"))
#' check_anapo_activity_consistent_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_anapo_activity_consistent_inspector <- function(dataframe1,
                                                      dataframe2,
                                                      output,
                                                      vessel_type = c("1", "2", "5", "6", "10")) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  vessel_code <- NULL
  vms_date <- NULL
  activity_date <- NULL
  nb_activity <- NULL
  vms_codevessel <- NULL
  vms_date <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type"),
    column_type = c("character", "Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type"),
      column_type = c("character", "Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "vessel_code"),
    column_type = c("character", "Date", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "vessel_code"),
      column_type = c("character", "Date", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "activity_date", "vessel_code")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$vms_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation number date
  dataframe2_nb_activity <- dataframe2 %>%
    dplyr::group_by(activity_date, vessel_code) %>%
    dplyr::summarise(nb_activity = dplyr::n(), .groups = "drop")
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2_nb_activity, by = dplyr::join_by(vms_date == activity_date, vessel_code == vessel_code))
  # Case of NA nb_activity and indicates when there are VMS-related activities
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      nb_activity = dplyr::coalesce(nb_activity, 0),
      logical = nb_activity > 0
    )
  # Case of vessel types not to be controlled
  dataframe1[!(dataframe1$vessel_type %in% vessel_type), "logical"] <- TRUE
  # Case of a VMS point assigned to several different vessels according to Observe/Turbobat (vessel_code) but originating from the same vessel according to VMS (vms_codevessel) (see link between Turbobat file and VMS)
  # If the match is valid (logical TRUE) for one of the vessel, it is assigned to all the others
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(vms_codevessel, vms_date) %>%
    dplyr::mutate(logical = ifelse(any(logical), TRUE, logical)) %>%
    dplyr::ungroup()
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, nb_activity, .after = logical)
  dataframe1 <- dplyr::relocate(.data = dataframe1, vms_date, .after = vessel_code) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$vms_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$vms_id[is.na(dataframe1$logical)], collapse = ", "))
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
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " VMS with no activity")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that checks the consistency of the various lists before filtering by the user
check_consistency_list <- function(sql_info, check_info, column_user_info, type_check_info, tab_info) {
  # 1 - Arguments verification ----
  name_file_sql <- sapply(sql_info, `[[`, "file")
  name_column_sql <- sapply(sql_info, `[[`, "column_user_id")
  name_rename_column_user_check <- sapply(check_info, `[[`, "rename_column_user")
  # Check tab_info arguments and retrieve all tab identifiers
  all_id_tab <- lapply(tab_info, function(tab) {
    # Check that the sublist contains an 'id' element
    if (!("id" %in% names(tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the tab because there is no element in the sub-list named 'id'.
   Present element : ",
        paste0(paste(names(tab), tab, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = tab[["id"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = tab[["id"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that element 'title' in the sub-list is character
    if (!("title" %in% names(tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Tabs must have a title, there is no element in the sub-list named 'title'.
   Present element : ",
        paste0(paste(names(tab), tab, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = tab[["title"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = tab[["title"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that element 'text' in the sub-list is character
    if ("text" %in% names(tab)) {
      if (!codama::r_type_checking(
        r_object = tab[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = tab[["text"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'display_dividing_lines' in the sub-list is logical
    if ("display_dividing_lines" %in% names(tab)) {
      if (!codama::r_type_checking(
        r_object = tab[["display_dividing_lines"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = tab[["display_dividing_lines"]],
          type = "logical",
          output = "error"
        ))
      }
    }
    return(tab[["id"]])
  })
  if (length(unlist(all_id_tab)) != length(unique(unlist(all_id_tab)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Tab id are not unique.",
      "\n Tab id : ",
      paste0(unlist(all_id_tab), collapse = ", "),
      sep = ""
    )
  }
  # Check type_check_info arguments and retrieve all choice identifiers
  # Check that the sublist contains an 'title' element
  if (!("title" %in% names(type_check_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Information is missing for the button used to select the check to be displayed. There is no element in the sub-list named 'title'.",
      "\n Present element : ",
      paste0(paste(names(type_check_info), type_check_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = type_check_info[["title"]],
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = type_check_info[["title"]],
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  all_id_choice_type_check <- lapply(type_check_info, function(choice) {
    if (is.list(choice)) {
      # Check that the sublist contains an 'id' element
      if (!("id" %in% names(choice))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Impossible to identify the choice because there is no element in the sub-list named 'id'.
   Present element : ",
          paste0(paste(names(choice), choice, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      if (!codama::r_type_checking(
        r_object = choice[["id"]],
        type = "character",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = choice[["id"]],
          type = "character",
          length = 1L,
          output = "error"
        ))
      }
      # Check that element 'text' in the sub-list is character
      if (!("text" %in% names(choice))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Tabs must have a text, there is no element in the sub-list named 'text'.
     Present element : ",
          paste0(paste(names(choice), choice, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      if (!codama::r_type_checking(
        r_object = choice[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = choice[["text"]],
          type = "character",
          output = "error"
        ))
      }
      # Check that element 'specific_check' in the sub-list is logical
      if ("specific_check" %in% names(choice)) {
        if (!codama::r_type_checking(
          r_object = choice[["specific_check"]],
          type = "logical",
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = choice[["specific_check"]],
            type = "logical",
            output = "error"
          ))
        }
      }
      return(choice[["id"]])
    }
  })
  if (length(unlist(all_id_choice_type_check)) != length(unique(unlist(all_id_choice_type_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Choice id of check types are not unique.",
      "\n Choice id : ",
      paste0(unlist(all_id_choice_type_check), collapse = ", "),
      sep = ""
    )
  }
  # Check check_info arguments and retrieve all tab identifiers
  all_id_check <- lapply(check_info, function(check) {
    # Check that the sublist contains an 'id' element
    if (!("id" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the check because there is no element in the sub-list named 'id'.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["id"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["id"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that the sublist contains an 'function_check' element
    if (!("function_check" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'function_check', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    # Check that element 'function_check' in the sub-list is function
    if (!inherits(x = check[["function_check"]], what = "function")) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - The sub-list named 'function_check' must be a function.",
        "\n check id : ",
        check[["id"]],
        "\n function_check : ",
        check[["function_check"]]
        ,
        sep = ""
      )
    }
    # Check that the sublist contains an 'argument_function_check' element
    if (!("argument_function_check" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'argument_function_check', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["argument_function_check"]],
      type = "list",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["argument_function_check"]],
        type = "list",
        output = "error"
      ))
    }
    # Check that the sublist contains an 'table_user_id' element
    if (!("table_user_id" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'table_user_id', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["table_user_id"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["table_user_id"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that the sublist contains an 'user_type' element
    if (!("user_type" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Information is missing for check. There is no element in the sub-list named 'user_type', mandatory.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["user_type"]],
      type = "character",
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["user_type"]],
        type = "character",
        output = "error"
      ))
    }
    # Check that element 'rename_column_user' in the sub-list is list
    if ("rename_column_user" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["rename_column_user"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["rename_column_user"]],
          type = "list",
          output = "error"
        ))
      }
    }
    # Check that the sublist contains an 'function_data_plot' element
    if ("function_data_plot" %in% names(check)) {
      # Check that element 'function_data_plot' in the sub-list is function
      if (!inherits(x = check[["function_data_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_data_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_data_plot : ",
          check[["function_data_plot"]]
          ,
          sep = ""
        )
      }
      # Check that the sublist contains an 'argument_function_data_plot' element
      if (!("argument_function_data_plot" %in% names(check))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - Information is missing for check. There is no element in the sub-list named 'argument_function_data_plot', mandatory if function_data_plot exist.",
          "\n Present element : ",
          paste0(paste(names(check), check, sep = " : "), collapse = ", "),
          sep = ""
        )
      }
      # Check that element 'argument_function_data_plot' in the sub-list is list
      if (!codama::r_type_checking(
        r_object = check[["argument_function_data_plot"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["argument_function_data_plot"]],
          type = "list",
          output = "error"
        ))
      }
    }
    if ("additional_column_user" %in% names(check)) {
      # Check that element 'additional_column_user' in the sub-list is character
      if (!codama::r_type_checking(
        r_object = check[["additional_column_user"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["additional_column_user"]],
          type = "character",
          output = "error"
        ))
      }
    }
    if ("function_display" %in% names(check)) {
      # Check that element 'function_display' in the sub-list is function
      if (!inherits(x = check[["function_display"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_display' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_display : ",
          check[["function_display"]]
          ,
          sep = ""
        )
      }
    }
    if ("argument_function_display" %in% names(check)) {
      # Check that element 'argument_function_display' in the sub-list is list
      if (!codama::r_type_checking(
        r_object = check[["argument_function_display"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["argument_function_display"]],
          type = "list",
          output = "error"
        ))
      }
    }
    if ("need_vms" %in% names(check)) {
      # Check that element 'need_vms' in the sub-list is logical
      if (!codama::r_type_checking(
        r_object = check[["need_vms"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["need_vms"]],
          type = "logical",
          output = "error"
        ))
      }
    }
    # Check that element 'title' in the sub-list is character
    if ("title" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["title"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["title"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'text' in the sub-list is character
    if ("text" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["text"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["text"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'type' in the sub-list is character
    if (!("type" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Check must have a type, there is no element in the sub-list named 'type'.",
        "\n Present element : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = check[["type"]],
      type = "character",
      allowed_value = unlist(all_id_choice_type_check),
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["type"]],
        type = "character",
        allowed_value = unlist(all_id_choice_type_check),
        output = "error"
      ))
    }
    # Check that element 'size_box' in the sub-list is character
    if ("size_box" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["size_box"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["size_box"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'column_no_wrap' in the sub-list is numeric
    if ("column_no_wrap" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["column_no_wrap"]],
        type = "numeric",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["column_no_wrap"]],
          type = "numeric",
          output = "error"
        ))
      }
    }
    # Check that element 'function_plot' in the sub-list is function
    if ("function_plot" %in% names(check)) {
      if (!inherits(x = check[["function_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_plot : ",
          check[["function_plot"]]
          ,
          sep = ""
        )
      }
    }
    # Check that element 'function_text_plot' in the sub-list is character
    if ("function_text_plot" %in% names(check)) {
      if (!inherits(x = check[["function_text_plot"]], what = "function")) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The sub-list named 'function_text_plot' must be a function.",
          "\n check id : ",
          check[["id"]],
          "\n function_text_plot : ",
          check[["function_text_plot"]]
          ,
          sep = ""
        )
      }
    }
    # Check that element 'title_window' in the sub-list is character
    if ("title_window" %in% names(check)) {
      if (!codama::r_type_checking(
        r_object = check[["title_window"]],
        type = "character",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = check[["title_window"]],
          type = "character",
          length = 1L,
          output = "error"
        ))
      }
    }
    # Check that the sublist contains an 'tab' element
    if (!("tab" %in% names(check))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the tab for check because there is no element in the sub-list named 'tab'.",
        "\n Control element available : ",
        paste0(paste(names(check), check, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    # Check that element 'tab' in the sub-list is character
    if (!codama::r_type_checking(
      r_object = check[["tab"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = check[["tab"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    if (!(check[["tab"]] %in% unlist(all_id_tab))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Invalid tab reference for check display.",
        "\n Check tab name : ",
        check[["tab"]],
        " for check id ", check[["id"]],
        "\n Tab name available : ",
        paste0(unlist(all_id_tab), collapse = ", "),
        sep = ""
      )
    }
    return(check[["id"]])
  })
  if (length(unlist(all_id_check)) != length(unique(unlist(all_id_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Check id are not unique.",
      "\n Check id : ",
      paste0(unlist(all_id_check), collapse = ", "),
      sep = ""
    )
  }
  # Check arguments in sql_info
  lapply(sql_info, function(sql) {
    # Check that the sublist contains an 'file' element
    if (!("file" %in% names(sql))) {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Impossible to identify the sql because there is no element in the sub-list named 'file'.",
        "\n Present element : ",
        paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
        sep = ""
      )
    }
    if (!codama::r_type_checking(
      r_object = sql[["file"]],
      type = "character",
      length = 1L,
      output = "logical"
    )) {
      return(codama::r_type_checking(
        r_object = sql[["file"]],
        type = "character",
        length = 1L,
        output = "error"
      ))
    }
    # Check that element 'anchor' in the sub-list is list
    if ("anchor" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["anchor"]],
        type = "list",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["anchor"]],
          type = "list",
          output = "error"
        ))
      }
    }
    # Check that element 'use_selection_other_sql' in the sub-list is list
    if ("use_selection_other_sql" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["use_selection_other_sql"]],
        type = "logical",
        length = 1L,
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["use_selection_other_sql"]],
          type = "logical",
          length = 1L,
          output = "error"
        ))
      }
      # Check that the sublist contains an 'column_anchor' element
      if (sql[["use_selection_other_sql"]] && (!("vector" %in% names(sql)) || !sql[["vector"]])) {
        if (!("column_anchor" %in% names(sql))) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Information is missing for SQL. There is no element in the sub-list named 'column_anchor', mandatory if use_selection_other_sql are TRUE and vector is FALSE.",
            "\n Present element : ",
            paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
            sep = ""
          )
        }
        if (!codama::r_type_checking(
          r_object = sql[["column_anchor"]],
          type = "character",
          length = 1L,
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = sql[["column_anchor"]],
            type = "character",
            length = 1L,
            output = "error"
          ))
        }
      }
    }
    # Check that element 'column_user_id' in the sub-list is list
    if ("column_user_id" %in% names(sql)) {
      if (!codama::r_type_checking(
        r_object = sql[["column_user_id"]],
        type = "character",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["column_user_id"]],
          type = "character",
          output = "error"
        ))
      }
    }
    # Check that element 'vector' in the sub-list is list
    if ("vector" %in% names(sql)) {
      # Check that element 'vector' in the sub-list is logical
      if (!codama::r_type_checking(
        r_object = sql[["vector"]],
        type = "logical",
        output = "logical"
      )) {
        return(codama::r_type_checking(
          r_object = sql[["vector"]],
          type = "logical",
          output = "error"
        ))
      }
    }
  })
  if (length(unlist(name_file_sql)) != length(unique(unlist(name_file_sql)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - File names must be unique.",
      "\n File names : ",
      paste0(unlist(name_file_sql), collapse = ", "),
      sep = ""
    )
  }
  # Check that the sublist contains an 'rename_id_column_user' element
  if (!("rename_id_column_user" %in% names(column_user_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - There is no element in the sub-list named 'rename_id_column_user' in list column_user_info.",
      "\n Present element : ",
      paste0(paste(names(column_user_info), column_user_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = column_user_info[["rename_id_column_user"]],
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = column_user_info[["rename_id_column_user"]],
      type = "list",
      output = "error"
    ))
  }
  if (!all(names(column_user_info[["rename_id_column_user"]]) %in% unlist(name_column_sql))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'rename_id_column_user' must also exist in at least one sub-list 'column_user_id', if the rename only concerns a specific check, then use the 'rename_column_user' sub-list.",
      "\n Problematic column names of sub-list named 'rename_id_column_user' : ",
      paste0(names(column_user_info[["rename_id_column_user"]])[!(names(column_user_info[["rename_id_column_user"]]) %in% unlist(name_column_sql))], collapse = ", "),
      sep = ""
    )
  }
  if (any(names(column_user_info[["rename_id_column_user"]]) %in% names(unlist(name_rename_column_user_check)))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'rename_id_column_user' must not also be indicated in a sub-list 'rename_column_user', either the column allows the user to identify the row, in which case use sub-list 'rename_id_column_user', or it is specific to a check, in which case use sub-list 'rename_column_user'.",
      "\n Problematic column names of sub-list named 'rename_id_column_user' : ",
      paste0(names(column_user_info[["rename_id_column_user"]])[names(column_user_info[["rename_id_column_user"]]) %in% names(unlist(name_rename_column_user_check))], collapse = ", "),
      sep = ""
    )
  }
  # Check that the sublist contains an 'order_id_column_user' element
  if (!("order_id_column_user" %in% names(column_user_info))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - There is no element in the sub-list named 'order_id_column_user' in list column_user_info.",
      "\n Present element : ",
      paste0(paste(names(column_user_info), column_user_info, sep = " : "), collapse = ", "),
      sep = ""
    )
  }
  if (!codama::r_type_checking(
    r_object = column_user_info[["order_id_column_user"]],
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = column_user_info[["order_id_column_user"]],
      type = "character",
      output = "error"
    ))
  }
  if (!all(names(column_user_info[["order_id_column_user"]]) %in% unlist(name_column_sql))) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The column names of sub-list named 'order_id_column_user' must also exist in at least one sub-list 'column_user_id'.",
      "\n Problematic column names of sub-list named 'order_id_column_user' : ",
      paste0(names(column_user_info[["order_id_column_user"]])[!(names(column_user_info[["order_id_column_user"]]) %in% unlist(name_column_sql))], collapse = ", "),
      sep = ""
    )
  }
}

# Shiny function : Error message if the trip selection elements are not correctly filled in
text_error_trip_select_server <- function(id, parent_in, config_data) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # if no selection element is filled in
      if (sum(isTruthy(parent_in$vessel_number), isTruthy(parent_in$trip_end_date)) == 0 && sum(isTruthy(parent_in$trip_start_date_range), isTruthy(parent_in$trip_end_date_range)) == 0) {
        return("Error: please select a trip")
      }
      # if there are elements filled in for several types of selection
      if (any(isTruthy(parent_in$vessel_number), isTruthy(parent_in$trip_end_date)) && any(isTruthy(parent_in$trip_start_date_range), isTruthy(parent_in$trip_end_date_range))) {
        return("Error: please choose only one type of trip selection")
      }
      # if there are missing selection elements for a selection type
      if (sum(isTruthy(parent_in$vessel_number), isTruthy(parent_in$trip_end_date)) == 1 || sum(isTruthy(parent_in$trip_start_date_range), isTruthy(parent_in$trip_end_date_range)) == 1) {
        return("Error: Please fill in the missing selection item")
      }
      # if the end date is earlier than the start date
      if (isTruthy(parent_in$trip_start_date_range) && isTruthy(parent_in$trip_end_date_range) && parent_in$trip_start_date_range > parent_in$trip_end_date_range) {
        return("Error: start date must be before end date")
      }
      # If the connection file is missing
      if (!isTruthy(config_data())) {
        text <- "Error: There is no configuration file for the connection to the base"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0(text, ", please either select one using the \"settings\" tab or put it in ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")))
      }
      # If the database selection is missing
      if (is.null(parent_in$`tab-data_base_observe`)) {
        return("Error: please select a database Observe")
      }
      # If connection information is missing for at least one base in the configuration file
      if (!all(parent_in$`tab-data_base_observe` %in% names(config_data()[["databases_configuration"]]))) {
        text <- paste0("Error: connection information for the database : ", paste0(parent_in$`tab-data_base_observe`[!parent_in$`tab-data_base_observe` %in% names(config_data()[["databases_configuration"]])], collapse = ", "), ", are not available in the connection file")
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0(text, ", please modify the configuration file or do not select this base"))
      }
      # If the check selection is missing
      if (is.null(parent_in[["tab-select_check"]])) {
        return("Error: please select at least 1 check")
      }
      return(TRUE)
    })
  })
}

# Shiny function : Read the .yml file of configuration for the connection
config_data_server <- function(id, parent_in) {
  moduleServer(id, function(input, output, session) {
    # Triggers reading of the configuration file when the user loads one (parent_in$setting_file_path) (also enables reading of the file when the application is launched via initialization of parent_in$setting_file_path) or when a calculation is restarted (input$start_button) (enables the user to take advantage of the new version available in .appconfig)
    eventReactive(c(input$start_button, parent_in$setting_file_path), {
      # If the user has not specified a file and the file exists in the default path, indicates the default path
      if (is.null(parent_in$setting_file_path$datapath) && file.exists(file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"))) {
        path_setting_file <- file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")
        # If the user has specified a file, indicates the path to specify
      } else if (!is.null(parent_in$setting_file_path$datapath)) {
        path_setting_file <- parent_in$setting_file_path$datapath
      }
      # Read the file if the path is available
      if (exists("path_setting_file")) {
        furdeb::configuration_file(
          path_file = path_setting_file,
          silent = TRUE
        )
      }
    })
  })
}

# Shiny function : Retrieves the list of trips and VMS selected by the user
trip_select_server <- function(id, parent_in, text_error_trip_select, config_data, sql_info_selected) {
  # 0 - Global variables assignement ----
  vms_id <- NULL
  vessel_code <- NULL
  vms_date <- NULL
  vms_codevessel <- NULL
  vessel_type <- NULL
  vessel_statut <- NULL
  # 1 - Data design ----
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # Recovery of reactive values
      sql_info <- sql_info_selected()[["sql_info_input_user"]]
      # If the connection data exists and there was no error in the trip selection, makes the connection
      req(config_data())
      if (text_error_trip_select() == TRUE) {
        config_observe_database <- config_data()[["databases_configuration"]]
        data_connection <- list()
        for (observe_database in config_observe_database[c(parent_in$`tab-data_base_observe`)]){
          data_connection <- append(data_connection, list(furdeb::postgresql_dbconnection(
            db_user = observe_database[["login"]],
            db_password = observe_database[["password"]],
            db_dbname = observe_database[["dbname"]],
            db_host = observe_database[["host"]],
            db_port = observe_database[["port"]]
          )))
        }
        cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql trip_selected \n", sep = "")
        trip_selected <- furdeb::data_extraction(
          type = "database",
          file_path = system.file("sql",
                                  "trip_selected.sql",
                                  package = "AkadoR"),
          database_connection = data_connection,
          anchor = list(select_item_1 = config_data()[["logbook_program"]],
                        select_item_2 = as.character(parent_in$vessel_number),
                        select_item_3 = parent_in$trip_end_date,
                        select_item_4 = parent_in$trip_start_date_range,
                        select_item_5 = parent_in$trip_end_date_range)
        )
        list_return <- list(trip_selected = trip_selected)
        if (dim(trip_selected)[1] > 0) {
          # If selected trip with the vessel code and the end date of the trip
          if (isTruthy(parent_in$vessel_number) && isTruthy(parent_in$trip_end_date)) {
            # VMS selection parameter date range by user-selected trip duration
            start_date_range <- min(trip_selected$trip_startdate)
            end_date_range <- max(trip_selected$trip_enddate)
            # VMS selection parameter vessel number by user-selected explicit
            vessel_number <- as.character(parent_in$vessel_number)
          }
          # If selected trip with a date range
          if (isTruthy(parent_in$trip_start_date_range) && isTruthy(parent_in$trip_end_date_range)) {
            # VMS selection parameter date range by user-selected period explicit
            start_date_range <- parent_in$trip_start_date_range
            end_date_range <- parent_in$trip_end_date_range
            vessel_number <- ""
          }
        }
        if (dim(trip_selected)[1] > 0 && "activity_vms" %in% sapply(sql_info, `[[`, "file")) {
          cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql activity_vms \n", sep = "")
          # Uses a function to extract data from VMS
          activity_vms <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "activity_vms.sql",
                                    package = "AkadoR"),
            database_connection = data_connection,
            anchor = list(select_item_1 = start_date_range, select_item_2 = end_date_range, select_item_3 = vessel_number)
          )
          list_return <- append(list_return, list(activity_vms = activity_vms))
        }
        # Disconnection to the base
        for (i in seq(from = 1, to = length(data_connection))) {
          DBI::dbDisconnect(data_connection[[i]][[2]])
        }
        if (dim(trip_selected)[1] > 0 && "vms" %in% sapply(sql_info, `[[`, "file")) {
          # If the database is "vms", read, transform and execute the SQL query that selects the trips according to the user parameters
          if (!is.null(config_data()[["vms_databases_configuration"]][["vms"]])) {
            # Connection to the base VMS
            data_connection_vms <- furdeb::postgresql_dbconnection(
              db_user = config_data()[["vms_databases_configuration"]][["vms"]][["login"]],
              db_password = config_data()[["vms_databases_configuration"]][["vms"]][["password"]],
              db_dbname = config_data()[["vms_databases_configuration"]][["vms"]][["dbname"]],
              db_host = config_data()[["vms_databases_configuration"]][["vms"]][["host"]],
              db_port = config_data()[["vms_databases_configuration"]][["vms"]][["port"]]
            )
            # Uses a function to extract data from VMS
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql vms \n", sep = "")
            vms <- furdeb::data_extraction(
              type = "database",
              file_path = system.file("sql",
                                      "vms.sql",
                                      package = "AkadoR"),
              database_connection = data_connection_vms,
              anchor = list(select_item_1 = start_date_range, select_item_2 = end_date_range, select_item_3 = vessel_number)
            )
            # Disconnection to the bases
            DBI::dbDisconnect(data_connection_vms[[2]])
            # Force date type, otherwise empty dataframe sets to charactere format
            vms$vms_date <- as.Date(vms$vms_date)
            # Removal of duplicate lines for date and vessel (id, time, position, ... not considered here)
            vms_route <- vms %>%
              dplyr::select(vms_id, vms_date, vessel_code, vms_codevessel, vessel_type, vessel_statut) %>%
              dplyr::distinct() %>%
              dplyr::relocate(vessel_code)
            list_return <- append(list_return, list(vms = vms, vms_route = vms_route))
          }
        }
        # If trips have been found return them otherwise return FALSE
        if (dim(trip_selected)[1] > 0) {
          return(list_return)
        } else {
          return(FALSE)
        }
      }
    })
  })
}

# Shiny function : Performs all calculations to test for inconsistencies
calcul_check_server <- function(id, text_error_trip_select, trip_select, config_data, referential_file, check_info_selected, sql_info_selected, column_user_info, parent_in) {
  moduleServer(id, function(input, output, session) {
    # 0 - Global variables assignement ----
    trip_id <- NULL
    eventReactive(trip_select(), {
      # 1 - Arguments verification ----
      # Recovery of reactive values
      check_info <- check_info_selected()
      sql_info <- sql_info_selected()[["sql_info"]]
      # Check arguments in sql_info
      name_file_sql <- sapply(sql_info, `[[`, "file")
      lapply(sql_info, function(sql) {
        # Check that the sublist contains an 'file' element
        if (!("file" %in% names(sql))) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Impossible to identify the sql because there is no element in the sub-list named 'file'.",
            "\n Present element : ",
            paste0(paste(names(sql), sql, sep = " : "), collapse = ", "),
            sep = ""
          )
        }
        if (!codama::r_type_checking(
          r_object = sql[["file"]],
          type = "character",
          length = 1L,
          output = "logical"
        )) {
          return(codama::r_type_checking(
            r_object = sql[["file"]],
            type = "character",
            length = 1L,
            output = "error"
          ))
        }
        if (sql[["file"]] %in% names(config_data())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the name of the parameters in the configuration file cannot be identical, as both are likely to be used as anchor in SQL queries, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Name parameters configuration file : ",
            paste0(names(config_data()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% names(referential_file())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the names of reference data sets cannot be identical, as both can be used as input to a check function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Names of reference data sets : ",
            paste0(names(referential_file()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% names(trip_select())) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the names of SQL initializing the user's query, as both can be used as input to a check function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Names of SQL initializing the user's query : ",
            paste0(names(trip_select()), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% c("trip_selected", "vessel_selected")) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the values from the SQL initializing the connection cannot be identical, as both are likely to be used as anchor in SQL queries, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Values from the SQL initializing connection : ",
            paste0(c("trip_selected", "vessel_selected"), collapse = ", "),
            sep = ""
          )
        }
        if (sql[["file"]] %in% c("check", "plot")) {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - The name of the SQL file and the values from the display function or data plot function cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
            "\n Name SQL file : ",
            sql[["file"]],
            "\n Values from the display function or data plot function : ",
            paste0(c("check", "plot"), collapse = ", "),
            sep = ""
          )
        }
        if (system.file("sql", paste0(sql[["file"]], ".sql"), package = "AkadoR") == "") {
          stop(
            format(
              x = Sys.time(),
              format = "%Y-%m-%d %H:%M:%S"
            ),
            " - Unable to find the following SQL file in package AkadoR",
            "\n Name SQL file : ",
            sql[["file"]],
            sep = ""
          )
        }
        # Check that element 'anchor' in the sub-list is list
        if ("anchor" %in% names(sql)) {
          if (!codama::r_type_checking(
            r_object = sql[["anchor"]],
            type = "list",
            output = "logical"
          )) {
            return(codama::r_type_checking(
              r_object = sql[["anchor"]],
              type = "list",
              output = "error"
            ))
          }
        }
        # Check that the anchor reference is correct
        if ("anchor" %in% names(sql)) {
          # For SQL that allows the extraction of other SQL (use_selection_other_sql is TRUE), only references to data frames initializing the user query or configuration file arguments are allowed
          if (!is.null(sql[["use_selection_other_sql"]]) && sql[["use_selection_other_sql"]]) {
            if (any(!c(unlist(sql[["anchor"]]) %in% c("trip_selected", "vessel_selected", names(config_data()))))) {
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - An anchor is invalid, for SQL that allows the extraction of other SQL (use_selection_other_sql are TRUE), only references to data frames initializing the user query or configuration file arguments are allowed.",
                "\n Anchor : ",
                paste0(paste(names(sql[["anchor"]]), sql[["anchor"]], sep = " : "), collapse = ", "),
                "\n References allowed : ",
                paste0(c("trip_selected", "vessel_selected", names(config_data())), collapse = ", "),
                sep = ""
              )
            }
          } else {
            # For SQL that doesn't allow the extraction of other SQL (use_selection_other_sql is FALSE), only references to data frames initializing the user query, configuration file arguments or the name of another SQL file (with use_selection_other_sql is TRUE, the column indicated in id can be used as anchor) are allowed
            if (any(!c(unlist(sql[["anchor"]]) %in% c("trip_selected", "vessel_selected", names(config_data()), name_file_sql[sapply(sql_info, `[[`, "use_selection_other_sql")])))) {
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - An anchor is invalid, for SQL that doesn't allow the extraction of other SQL (use_selection_other_sql is FALSE), only references to data frames initializing the user query, configuration file arguments or the name of another SQL file (with use_selection_other_sql is TRUE, the column indicated in id can be used as anchor) are allowed.",
                "\n Anchor : ",
                paste0(paste(names(sql[["anchor"]]), sql[["anchor"]], sep = " : "), collapse = ", "),
                "\n References allowed : ",
                paste0(c("trip_selected", "vessel_selected", names(config_data()), sapply(sql_info, `[[`, "file")), collapse = ", "),
                sep = ""
              )
            }
          }
        }
      })
      if (any(names(trip_select()) %in% c("check", "plot"))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The names of SQL initializing the user's query and the values from the display function or data plot function cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          "\n Values from the display function or data plot function : ",
          paste0(c("check", "plot"), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(trip_select()) %in% names(referential_file()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The names of SQL initializing the user's query and the names of reference data sets cannot be identical, as both can be used as input to a display function or data plot function, leading to a conflict.",
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          "\n Names of reference data sets : ",
          paste0(names(referential_file()), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(config_data()) %in% names(trip_select()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The name of the parameters in the configuration file and the names of SQL initializing the user's query cannot be identical, as both can be used as input to a check function, leading to a conflict.",
          "\n Name of the parameters in the configuration file : ",
          paste0(names(config_data()), collapse = ", "),
          "\n Names of SQL initializing the user's query : ",
          paste0(names(trip_select()), collapse = ", "),
          sep = ""
        )
      }
      if (any(names(config_data()) %in% names(referential_file()))) {
        stop(
          format(
            x = Sys.time(),
            format = "%Y-%m-%d %H:%M:%S"
          ),
          " - The name of the parameters in the configuration file and the names of reference data sets cannot be identical, as both can be used as input to a check function, leading to a conflict.",
          "\n Name of the parameters in the configuration file : ",
          paste0(names(config_data()), collapse = ", "),
          "\n Names of reference data sets : ",
          paste0(names(referential_file()), collapse = ", "),
          sep = ""
        )
      }
      # 2 - Data extraction ----
      # If there was no error in the trip selection and that there are trips for user settings, performs consistency tests
      if (text_error_trip_select() == TRUE && !is.logical(trip_select())) {
        # If the selected controls use data from SQL indicate this in sql_info
        if (!identical(sql_info, list())) {
          # Connection to the base
          config_observe_database <- config_data()[["databases_configuration"]]
          data_connection <- list()
          for (observe_database in config_observe_database[c(parent_in$`tab-data_base_observe`)]){
            data_connection <- append(data_connection, list(furdeb::postgresql_dbconnection(
              db_user = observe_database[["login"]],
              db_password = observe_database[["password"]],
              db_dbname = observe_database[["dbname"]],
              db_host = observe_database[["host"]],
              db_port = observe_database[["port"]]
            )))
          }
          # Retrieve only data that will also be used to retrieve other SQLs
          data_select_sql <- lapply(stats::setNames(sql_info[unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))], paste0("data_", sapply(sql_info, `[[`, "file"))[unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))]), function(sql) {
            # Print message
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql id ", sql[["file"]], " \n", sep = "")
            # Retrieves anchor values to be supplied to SQL
            if (!is.null(sql[["anchor"]])) {
              anchor <- lapply(stats::setNames(sql[["anchor"]], names(sql[["anchor"]])), function(anchor) {
                if (anchor == "trip_selected") {
                  # Recovers trip_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$trip_id
                } else if (anchor == "vessel_selected") {
                  # Recovers vessel_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$vessel_id
                } else if (anchor %in% names(config_data())) {
                  # Retrieves parameter values from configuration file
                  config_data()[[anchor]]
                }
              })
            } else {
              anchor <- NULL
            }
            # Execute SQL query
            data <- furdeb::data_extraction(type = "database",
                                            file_path = system.file("sql",
                                                                    paste0(sql[["file"]], ".sql"),
                                                                    package = "AkadoR"),
                                            database_connection = data_connection,
                                            anchor = anchor)
            # Checks that the name of the column to be used to supply values when used in other SQL anchors exists in the dataset
            if (!(sql[["column_anchor"]] %in% colnames(data)) && (!("vector" %in% names(sql)) || !sql[["vector"]])) {
              # Disconnection to the bases
              for (i in seq(from = 1, to = length(data_connection))) {
                DBI::dbDisconnect(data_connection[[i]][[2]])
              }
              stop(
                format(
                  x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"
                ),
                " - The SQL ", sql[["file"]], " file does not contain the ", sql[["column_anchor"]], " column indicated in sub-list named 'column_anchor'.",
                sep = ""
              )
            }
            # Transforms dataframe into vector if vector argument is TRUE
            if (!is.null(sql[["vector"]]) && sql[["vector"]]) {
              if (ncol(data) == 1) {
                data <- dplyr::pull(data)
              } else {
                # Disconnection to the bases
                for (i in seq(from = 1, to = length(data_connection))) {
                  DBI::dbDisconnect(data_connection[[i]][[2]])
                }
                stop(
                  format(
                    x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"
                  ),
                  " - The SQL ", sql[["file"]], " file cannot be transformed into a vector because there are ", ncol(data), " columns",
                  "\n The sub-list named 'vector' must not be TRUE",
                  sep = ""
                )
              }
            }
            return(data)
          })
          # Retrieve only data that will not be used to retrieve other SQL
          data_sql <- lapply(stats::setNames(sql_info[!unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))], paste0("data_", sapply(sql_info, `[[`, "file"))[!unlist(sapply(sql_info, `[[`, "use_selection_other_sql"))]), function(sql) {
            # Print message
            cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start sql id ", sql[["file"]], " \n", sep = "")
            # Retrieves anchor values to be supplied to SQL
            if (!is.null(sql[["anchor"]])) {
              anchor <- lapply(stats::setNames(sql[["anchor"]], names(sql[["anchor"]])), function(anchor) {
                if (anchor == "trip_selected") {
                  # Recovers trip_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$trip_id
                } else if (anchor == "vessel_selected") {
                  # Recovers vessel_id values available directly in the SQL initializing the user's request
                  trip_select()$trip_selected$vessel_id
                } else if (anchor %in% names(config_data())) {
                  # Retrieves parameter values from configuration file
                  config_data()[[anchor]]
                } else {
                  # Retrieves the name of the column to be used as the anchor value
                  name_id <- sql_info[[which(sapply(sql_info, `[[`, "file") == anchor)]][["column_anchor"]]
                  if (!is.null(name_id)) {
                    # Extract anchor values in other data SQL
                    data_select_sql[[paste0("data_", anchor)]][[name_id]]
                  } else {
                    # Extract anchor values in other vector SQL
                    data_select_sql[[paste0("data_", anchor)]]
                  }
                }
              })
            } else {
              anchor <- NULL
            }
            # Execute SQL query
            data <- furdeb::data_extraction(type = "database",
                                            file_path = system.file("sql",
                                                                    paste0(sql[["file"]], ".sql"),
                                                                    package = "AkadoR"),
                                            database_connection = data_connection,
                                            anchor = anchor)
            # Transforms dataframe into vector if vector argument is TRUE
            if (!is.null(sql[["vector"]]) && sql[["vector"]]) {
              if (ncol(data) == 1) {
                data <- dplyr::pull(data)
              } else {
                # Disconnection to the bases
                for (i in seq(from = 1, to = length(data_connection))) {
                  DBI::dbDisconnect(data_connection[[i]][[2]])
                }
                stop(
                  format(
                    x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"
                  ),
                  " - The SQL ", sql[["file"]], " file cannot be transformed into a vector because there are ", ncol(data), " columns",
                  "\n The sub-list named 'vector' must not be TRUE",
                  sep = ""
                )
              }
            }
            return(data)
          })
          data_sql <- c(data_sql, data_select_sql)
          rm(data_select_sql)
          # Disconnection to the bases
          for (i in seq(from = 1, to = length(data_connection))) {
            DBI::dbDisconnect(data_connection[[i]][[2]])
          }
        } else {
          data_sql <- list()
        }
        # 3 - Data design ----
        if (!is.null(data_sql$data_previous_trip)) {
          # Reconstructs info from previous trips in different databases
          if (!codama::r_table_checking(
            r_table = data_sql$data_previous_trip,
            type = "data.frame",
            column_name = c("trip_previous_id", "trip_id", "vessel_code", "trip_enddate", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous", "harbour_id_landing", "harbour_label_landing"),
            column_type = c("character", "character", "character", "Date", "character", "character", "character", "character"),
            output = "logical"
          )) {
            codama::r_table_checking(
              r_table = data_sql$data_previous_trip,
              type = "data.frame",
              column_name = c("trip_previous_id", "trip_id", "vessel_code", "trip_enddate", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous", "harbour_id_landing", "harbour_label_landing"),
              column_type = c("character", "character", "character", "Date", "character", "character", "character", "character"),
              output = "error"
            )
          }
          for (i in data_sql$data_previous_trip[is.na(data_sql$data_previous_trip$trip_previous_id), "trip_id", drop = TRUE]) {
            # Recovers info from trip that has no previous trip
            current_data <- data_sql$data_previous_trip[data_sql$data_previous_trip$trip_id %in% i, ]
            # Search for trips from the same vessel and with a date lower than the current trip
            date_previous_trip <- data_sql$data_previous_trip[data_sql$data_previous_trip$vessel_code %in% current_data$vessel_code & current_data$trip_enddate > data_sql$data_previous_trip$trip_enddate, "trip_enddate", drop = TRUE]
            if (length(date_previous_trip) > 0) {
              date_min_full_trip <- max(date_previous_trip, na.rm = TRUE)
              previous_trip <- data_sql$data_previous_trip[data_sql$data_previous_trip$vessel_code %in% current_data$vessel_code & data_sql$data_previous_trip$trip_enddate == date_min_full_trip, ]
              # Assigns information from previous trip
              data_sql$data_previous_trip[data_sql$data_previous_trip$trip_id %in% i, c("trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous")] <- previous_trip[, c("trip_id", "harbour_id_landing", "harbour_label_landing"), drop = TRUE]
            }
          }
          # Remove trips not selected by the user but which have been useful for finding previous trips located in another database
          data_sql$data_previous_trip <- data_sql$data_previous_trip %>%
            dplyr::filter(trip_id %in% trip_select()$trip_selected$trip_id)
          # Checks data consistency
          if (nrow(data_sql$data_previous_trip) != length(trip_select()$trip_selected$trip_id)) {
            warning(text_object_more_or_less(id = trip_select()$trip_selected$trip_id, result_check = data_sql$data_previous_trip$trip_id))
          }
        }
        if (!is.null(data_sql$data_full_trip)) {
          # Reconstructs full trips from different databases
          if (!codama::r_table_checking(
            r_table = data_sql$data_full_trip,
            type = "data.frame",
            column_name = c("trip_end_full_trip_id", "trip_id", "vessel_id", "trip_enddate"),
            column_type = c("character", "character", "character", "Date"),
            output = "logical"
          )) {
            codama::r_table_checking(
              r_table = data_sql$data_full_trip,
              type = "data.frame",
              column_name = c("trip_end_full_trip_id", "trip_id", "vessel_id", "trip_enddate"),
              column_type = c("character", "character", "character", "Date"),
              output = "error"
            )
          }
          for (i in data_sql$data_full_trip[is.na(data_sql$data_full_trip$trip_end_full_trip_id) & !is.na(data_sql$data_full_trip$trip_id), "trip_id", drop = TRUE]) {
            # Recover trip info that doesn't belong to a finished full trip
            current_data <- data_sql$data_full_trip[data_sql$data_full_trip$trip_id %in% i, ]
            # Search for trips on the same vessel, in a finished full trip and with a date later than the current trip
            date_full_trip <- data_sql$data_full_trip[data_sql$data_full_trip$vessel_id %in% current_data$vessel_id & !is.na(data_sql$data_full_trip$trip_end_full_trip_id) & current_data$trip_enddate <= data_sql$data_full_trip$trip_enddate, "trip_enddate", drop = TRUE]
            if (length(date_full_trip) > 0) {
              date_min_full_trip <- min(date_full_trip, na.rm = TRUE)
              # Assigns the identifier of the finished full trip
              data_sql$data_full_trip[data_sql$data_full_trip$trip_id %in% i, "trip_end_full_trip_id"] <- data_sql$data_full_trip[data_sql$data_full_trip$vessel_id %in% current_data$vessel_id & !is.na(data_sql$data_full_trip$trip_end_full_trip_id) & data_sql$data_full_trip$trip_enddate == date_min_full_trip, "trip_end_full_trip_id", drop = TRUE]
            }
          }
        }
        # Execute check
        table_finish <- sapply(check_info, function(check) {
          if (!is.null(check[["function_check"]])) {
            # If the control needs VMS but the connection to the VMS database could not be made to create the data_vms dataset, then the control is not carried out
            if (!is.null(check[["need_vms"]]) && check[["need_vms"]] && !("vms" %in% names(trip_select()))) {
              return(stats::setNames(list(data.frame()), check[["id"]]))
            } else {
              # Print message
              cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check id ", check[["id"]], " \n", sep = "")
              tryCatch({
                # Finds the data frames to supply as arguments to the control function
                argument <- lapply(stats::setNames(check[["argument_function_check"]], names(check[["argument_function_check"]])), function(argument) {
                  if (paste0("data_", argument) %in% names(data_sql)) {
                    # Extract data from data SQL
                    data_sql[[paste0("data_", argument)]]
                  } else if (argument %in% names(referential_file())) {
                    # Retrieves parameter values from referential file
                    referential_file()[[argument]]
                  } else if (argument %in% names(config_data())) {
                    # Retrieves parameter values from configuration file
                    config_data()[[argument]]
                  } else if (argument %in% names(trip_select())) {
                    # Extract data from data SQL retrieved directly via user selections
                    trip_select()[[argument]]
                  } else {
                    argument
                  }
                })
                # Executes the control function
                check_result <- do.call(check[["function_check"]], argument)
                # Save in a new variable the data frame that will serve as the basis for the table displayed in the shiny application
                if (inherits(x = check_result, what = "list")) {
                  table_result <- check_result[[1]]
                } else {
                  table_result <- check_result
                }
                # Executes the function that modifies the table display specifically for this check
                if (!is.null(check[["function_display"]])) {
                  if (!is.null(check[["argument_function_display"]])) {
                    # Finds the data frames to supply as arguments to the display function
                    argument <- lapply(stats::setNames(check[["argument_function_display"]], names(check[["argument_function_display"]])), function(argument) {
                      if (paste0("data_", argument) %in% names(data_sql)) {
                        # Extract data
                        data_sql[[paste0("data_", argument)]]
                      } else if (argument == "check") {
                        table_result
                      } else if (argument == "plot") {
                        check_result[[2]]
                      } else {
                        argument
                      }
                    })
                    table_result <- do.call(check[["function_display"]], argument)
                  } else {
                    table_result <- do.call(check[["function_display"]], list(table_result))
                  }
                }
                if (!is.null(check[["function_plot"]])) {
                  if (!is.null(check[["function_data_plot"]])) {
                    # Finds the data frames to supply as arguments to the data plot function
                    argument <- lapply(stats::setNames(check[["argument_function_data_plot"]], names(check[["argument_function_data_plot"]])), function(argument) {
                      if (paste0("data_", argument) %in% names(data_sql)) {
                        # Extract data
                        data_sql[[paste0("data_", argument)]]
                      } else if (argument %in% names(trip_select())) {
                        trip_select()[[argument]]
                      } else if (argument == "check") {
                        table_result
                      } else if (argument == "plot") {
                        check_result[[2]]
                      } else {
                        argument
                      }
                    })
                    # Executes the data plot function
                    data_plot <- do.call(check[["function_data_plot"]], argument)
                  } else {
                    data_plot <- check_result[[2]]
                  }
                  # Name of the table containing the plot information in calcul_check_server
                  if (nrow(table_result) > 0) {
                    table_result$name_table <- check[["id"]]
                    # Add button and data for plot in table
                    argument <- list(id = check[["id"]], data = table_result, colname_id = grep("_id$", colnames(table_result), value = TRUE), colname_info = "name_table")
                    if (!is.null(check[["choice_display_plot"]])) {
                      argument <- c(argument, choice_select_row = check[["choice_display_plot"]])
                    }
                    table_result <- do.call(data_button_plot, argument)
                  }
                }
                # Finds the table containing the information needed for the user to identify the rows
                if (paste0("data_", check[["table_user_id"]]) %in% names(data_sql)) {
                  # Extract data in data_sql
                  table_display_data_info <- data_sql[[paste0("data_", check[["table_user_id"]])]][,  c(sql_info[[which(sapply(sql_info, `[[`, "file") == check[["table_user_id"]])]][["column_user_id"]], check[["additional_column_user"]])]
                } else if (check[["table_user_id"]] %in% names(trip_select())) {
                  # Extract data in trip_select
                  table_display_data_info <- trip_select()[[check[["table_user_id"]]]][,  c(check[["additional_column_user"]])]
                } else {
                  stop(
                    format(
                      x = Sys.time(),
                      format = "%Y-%m-%d %H:%M:%S"
                    ),
                    " - The sub-list named 'table_user_id' must be a name of the file SQL (", paste0(names(data_sql), collapse = ", "), ") or a name from the SQL initializing the user's query trip_select() (", paste0(names(trip_select()), collapse = ", "), ")",
                    "\n check id : ",
                    check[["id"]],
                    "\n table_user_id : ",
                    check[["table_user_id"]]
                    ,
                    sep = ""
                  )
                }
                # Uses a function to format the table (adds data_info columns allowing the user to identify rows (predefined column depending on the table specified in table_user_id for control purposes, with the possibility of adding columns in additional_column_user but which must already be present in the SQL linked to table_user_id) and modifies the display of certain columns (addition of icon, coordinate conversion, rename))
                table_result <- table_display(data = table_result, data_info = table_display_data_info, type_inconsistency = check[["type"]], rename = column_user_info[["rename_id_column_user"]], order = column_user_info[["order_id_column_user"]])
                # Modify the table for display purposes: rename column
                if (!is.null(check[["rename_column_user"]])) {
                  table_result <- table_result %>% dplyr::rename_with(~ unlist(check[["rename_column_user"]]), names(check[["rename_column_user"]]))
                }
                # return data check and data plot check if exist
                if (!is.null(check[["function_data_plot"]])) {
                  return(stats::setNames(list(list(table = table_result, list_plot = data_plot)), check[["id"]]))
                } else {
                  return(stats::setNames(list(list(table = table_result)), check[["id"]]))
                }
              }, error = function(error_message) {
                message("Error, check failure: ", conditionMessage(error_message))
                return(stats::setNames(list(list(error = (paste("Control failure :  ", error_message)))), check[["id"]]))
              })
            }
          }
        })
        table_finish
      }
    })
  })
}

# Shiny function : Displays the errors and notifications that occur when you want to start the calculation
error_trip_select_serveur <- function(id, text_error_trip_select, config_data, trip_select, calcul_check, parent_in) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText({
      # If there are errors in the selection parameters
      if (is.character(text_error_trip_select())) {
        showNotification(id = "notif_warning", ui = text_error_trip_select(), type = "error")
        return(paste0("<span style=\"color:red\">", text_error_trip_select(), "</span>"))
      }
      # If the selected trip is not found in the database
      if (text_error_trip_select() == TRUE && !is.list(trip_select()) && trip_select() == FALSE) {
        text <- "Error: no trip was found for these parameters"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0("<span style=\"color:red\">", text, "</span>"))
      }
      # If the different manipulations on the data are finished
      if (isTruthy(calcul_check())) {
        text <- "Calculation finished"
        showNotification(id = "notif_default", ui = text, type = "default")
        cat("\033[0;32m", format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Process AkadoR ran successfully.\n", "\033[0m", sep = "")
        return(paste0("<span style=\"color:#34C909\">", text, "</span>"))
      }
    })
  })
}

# Shiny function : Selection window for choosing the type of file to download
window_button_download <- function(name) {
  modalDialog(downloadButton(outputId = "download_csv", label = "CSV"),
              downloadButton(outputId = "download_excel", label = "Excel"),
              fade = TRUE,
              easyClose = TRUE,
              footer = NULL,
              title = "Download Table")
}

# Shiny function : creation tab, menu and content
tab <- function(id, tab_info, check_info, type_check_info, calcul_check, referential_file, config_data, res_auth) {
  # 1 - Arguments verification ----
  if (!codama::r_type_checking(
    r_object = id,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = id,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Instantiating the radio button to select the display of controls
  mod_radiobuttons_type_check_server(id = id, type_check_info = type_check_info)
  # Instantiating the menu and retrieving reactive values
  reactive_value_menu <- mod_tab_menu_server(id = id, tab_info = tab_info)
  # Instantiating the tab
  mod_tab_content_server(id = id, tab_info = tab_info, check_info = check_info, type_check_info = type_check_info, config_data = config_data, res_auth = res_auth)
  # Creation of all tables
  lapply(
    check_info,
    function(check) {
      do.call(mod_table_server, c(check[names(check) %in% names(formals(mod_table_server))], data_all = calcul_check, type_line_check = reactive_value_menu$type_line_check, referential_file = referential_file))
    }
  )
}

#' @name table_display
#' @title Transforms table display
#' @description Shiny function : Function that formats the display of check results, positions, row order and column names
#' @param data {\link[base]{data.frame}} expected. data frame containing check information
#' @param data_info {\link[base]{data.frame}} expected. data frame containing information to enable the user to identify lines
#' @param type_inconsistency {\link[base]{character}} expected. Control type. You can choose between "error", "warning" or "info".
#' @param rename {\link[base]{list}} expected. List of column names to be renamed (the left part is the column names, the right part is the new names)
#' @param order {\link[base]{character}} expected. Column names according to which rows are to be sorted.
#' @returns The function returns a {\link[base]{data.frame}}
#' @export
table_display <- function(data, data_info, type_inconsistency, rename, order) {
  # 0 - Global variables assignement ----
  . <- NULL
  `:=` <- NULL
  X <- NULL
  Y <- NULL
  button <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c("logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c("logical"),
      output = "error"
    )
  }
  if (!codama::r_table_checking(
    r_table = data_info,
    type = "data.frame",
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data_info,
      type = "data.frame",
      output = "error"
    )
  }
  if (!codama::r_type_checking(
    r_object = type_inconsistency,
    type = "character",
    allowed_value = c("error", "warning", "info"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = type_inconsistency,
      type = "character",
      allowed_value = c("error", "warning", "info"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = rename,
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = rename,
      type = "list",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = order,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = order,
      type = "character",
      output = "error"
    ))
  }
  # 2 - Data design ----
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Deletes duplicate columns
  all_colname <- c(colnames(data), colnames(data_info))
  colname_double <- table(all_colname)[table(all_colname) > 1]
  colname_double <- names(colname_double)[!(names(colname_double) %in% colname_id)]
  data <- data[, !(colnames(data) %in% colname_double)]
  # Combines the consistency test on the data and data identification information
  data <- dplyr::inner_join(data_info, data, by = dplyr::join_by(!!colname_id))
  # Add icons according to the success of the test
  data$logical[data$logical == TRUE] <- as.character(icon("check"))
  if (type_inconsistency == "error") {
    data$logical[data$logical == FALSE] <- as.character(icon("xmark"))
  }
  if (type_inconsistency == "warning") {
    data$logical[data$logical == FALSE] <- as.character(icon("exclamation"))
  }
  if (type_inconsistency == "info") {
    data$logical[data$logical == FALSE] <- as.character(icon("info"))
  }
  # Changes position display for all column position, converting DD (Decimal Degrees) coordinates in DDM (Degrees, Decimal Minutes)
  column_name_position <- grep("_position", colnames(data), value = TRUE)
  for (column in column_name_position){
    # Formats spatial data position
    data_geo <- data %>%
      dplyr::filter(!is.na(!!as.name(column))) %>%
      dplyr::select(column, colname_id) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(wkt = column, crs = 4326, remove = FALSE) %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
    new_name_column <- paste0(column, "_ddm")
    # Change position display format from decimal degrees to degrees, minutes, and decimal seconds
    data_geo <- data_geo %>%
      dplyr::mutate(!!new_name_column := paste0(coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE), "<br>", coordinate_dd_to_dmd(coordinate = X, latitude = FALSE)))
    data <- dplyr::left_join(data, data.frame(data_geo)[, c(colname_id, new_name_column)], by = dplyr::join_by(!!colname_id))
    data <- data %>%
      dplyr::relocate(new_name_column, .after = column) %>%
      dplyr::select(-column)
  }
  # Sort rows
  order_tmp <- order[order %in% colnames(data)]
  data <- data %>% dplyr::arrange_at(order_tmp)
  # Rename column
  rename_tmp <- rename[names(rename) %in% colnames(data)]
  data <- data %>% dplyr::rename_with(~ unlist(rename_tmp), names(rename_tmp))
  # Modify the table for display purposes specifically for logical : rename column
  if (length(grep("logical", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      Check = logical
    )
  }
  # Modify the table for display purposes specifically for button : rename column
  if (length(grep("button", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `Details problem` = button
    )
  }
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Modify the table for display purposes: delete column
  data <- subset(data, select = -c(eval(parse(text = colname_id))))
  return(data)
}

# Function to create a data.frame in list
data_to_list <- function(data, name_col_dataplot, colname_id, colname_plot, colname_info, rename_colname_info) {
  # 1 - Arguments verification ----
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

#' @name data_button_plot
#' @title Create the button in the table
#' @description Shiny function : Function to create the button in the table that will create the plot
#' @param id {\link[base]{character}} expected. Button identifier
#' @param data {\link[base]{data.frame}} expected. data frame where the button will be added
#' @param colname_id {\link[base]{character}} expected. Name of column used as unique identifier for data rows
#' @param colname_info {\link[base]{character}} expected. Name of data columns to be transmitted via button name
#' @param name_button {\link[base]{character}} expected. Default values: NULL, allows you to specify a name for the button, otherwise use the prefix "button_" and the supplied id
#' @param choice_select_row {\link[base]{character}} expected. Default values: "error", the possible values are "all" to create a button on all lines, "error" to create a button only on lines considered inconsistent, "valid" to create a button only on lines considered consistent
#' @returns The function returns a {\link[base]{data.frame}}
#' @export
data_button_plot <- function(id, data, colname_id, colname_info, name_button = NULL, choice_select_row = "error") {
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
    r_object = colname_id,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = colname_id,
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
    r_object = name_button,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = name_button,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = choice_select_row,
    type = "character",
    allowed_value = c("all", "error", "valid"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = choice_select_row,
      type = "character",
      allowed_value = c("all", "error", "valid"),
      output = "error"
    ))
  }
  if (!codama::r_table_checking(
    r_table = data,
    type = "data.frame",
    column_name = c(colname_id, colname_info),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = data,
      type = "data.frame",
      column_name = c(colname_id, colname_info),
      output = "error"
    )
  }
  # 2 - Data design ----
  # Select the lines that will display a plot
  if (choice_select_row == "all") {
    select_row <- rep(TRUE, nrow(data))
  }
  if (choice_select_row == "error") {
    select_row <- data$logical == FALSE
  }
  if (choice_select_row == "valid") {
    select_row <- data$logical == TRUE
  }
  data <- data %>% dplyr::mutate(button = NA)
  if (is.null(name_button)) {
    name_button <- paste0("button_", id)
  }
  # Add button in table
  if (any(select_row)) {
    data$button[select_row] <- sapply(which(select_row), function(num_row) {
      as.character(shiny::actionButton(inputId = paste0(data[num_row, c(colname_info, colname_id)], collapse = "&"), label = "Detail", onclick = paste0('Shiny.setInputValue(\"', shiny::NS(namespace = id, id = name_button), '\", this.id, {priority: \"event\"})')))
    })
  }
  data <- data %>% dplyr::select(!c(colname_info))
  return(data)
}

# Function to change table display raising_factor
display_raising_factor <- function(dataframe1) {
  dataframe1$rf1 <- trunc(dataframe1$rf1 * 100000) / 100000
  return(dataframe1)
}

# Function to change table display little_big
display_little_big <- function(dataframe1) {
  dataframe1$little_percentage <- trunc(dataframe1$little_percentage * 1000) / 10
  dataframe1$big_percentage <- trunc(dataframe1$big_percentage * 1000) / 10
  dataframe1$measure1_percentage <- trunc(dataframe1$measure1_percentage * 1000) / 10
  dataframe1$measure2_percentage <- trunc(dataframe1$measure2_percentage * 1000) / 10
  return(dataframe1)
}


# Function to create list data/argument for the plot plot_temporal_limit
plot_temporal_limit_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  trip_startdate <- NULL
  trip_enddate <- NULL
  # 1 - Data design ----
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


# Function to create the plot of the consistency of the dates by trip
plot_temporal_limit <- function(data, startdate, enddate) {
  # Deletes the rows where the date of the activity is missing
  data <- data[!is.na(data$activity_date), ]
  # Plot
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

# Function to create the windows with plot of the consistency of the dates by trip
plot_temporal_limit_windows <- function(vessel_code, enddate) {
  paste0("<b>Trip information : </b><br>
          <ul><li>Vessel code : ", vessel_code, "</li>
          <li>Trip end date : ", enddate, "</li></ul>")
}


# Function to create list data/argument for the plot plot_position
plot_position_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  . <- NULL
  activity_position <- NULL
  X <- NULL
  Y <- NULL
  activity_id <- NULL
  # 1 - Data design ----
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

# Function to create the plot of the consistency of the position for the activity
plot_position <- function(data) {
  # Local binding global variables
  . <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # Spatial formatting
  if (!is.na(data$activity_position)) {
    data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = unique(data$activity_crs)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
      dplyr::mutate(longitude = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), latitude = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE)) %>%
      dplyr::mutate(text = paste("Position:", latitude, ",", longitude, "<extra></extra>"))
    # Plot
    plotly::plot_ly(data = data_geo, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", marker = list(size = 10), hovertemplate = ~text) %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron", center = list(lon = data_geo$X, lat = data_geo$Y)))
  }else {
    # Plot
    plotly::plot_ly(type = "scattermapbox", mode = "markers", marker = list(size = 10)) %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron"))
  }
}

# Function to create the windows with plot of the consistency of the position for the activity
plot_position_windows <- function(vessel_code, trip_enddate, activity_date, activity_number, Y, X, type, ocean_label, ocean_calculate) {
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

# Function to create list data/argument for the plot plot_eez
plot_eez_data <- function(dataframe1, dataframe2) {
  # 0 - Global variables assignement ----
  . <- NULL
  activity_position <- NULL
  X <- NULL
  Y <- NULL
  activity_id <- NULL
  # 1 - Data design ----
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

# Function to create the plot of the consistency of the eez for the activity
plot_eez <- function(data, referential_geographical_shape) {
  # Local binding global variables
  . <- NULL
  ISO_TER1 <- NULL
  ISO_TER2 <- NULL
  ISO_TER3 <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  # Remove empty geometry
  referential_geographical_shape <- referential_geographical_shape[!sf::st_is_empty(referential_geographical_shape), ]
  # text hovertemplate
  referential_geographical_shape <- referential_geographical_shape %>% dplyr::mutate(text = paste("Country 1:", ISO_TER1, "<br>Country 2:", ISO_TER2, "<br>Country 3:", ISO_TER3, "<extra></extra>"))
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

# Function to create the windows with plot of the consistency of the eez for the activity
plot_eez_windows <- function(vessel_code, trip_enddate, activity_date, activity_number, Y, X, fpazone_code, fpazone_country_iso3, eez_calculated) {
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

# Function to change table display anapo
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
  # 1 - Data design ----
  check_anapo_inspector_dataplot <- dplyr::inner_join(dataframe1, dataframe2[, c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_number", "vesselactivity_code")], by = dplyr::join_by(activity_id))
  # Add information on whether the activity is linked to a grounding (object or buoy) or not in data plot
  data_tmp_grounding <- column_grounding(data = check_anapo_inspector_dataplot, data_transmittingbuoy = dataframe3)
  check_anapo_inspector_dataplot <- dplyr::inner_join(check_anapo_inspector_dataplot, data_tmp_grounding, by = dplyr::join_by(activity_id))
  # Selecting useful data for the plot
  check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot %>%
    dplyr::select("trip_id", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "vesselactivity_code", "activity_crs", "grounding") %>%
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


# Function to create list data/argument for the plot plot_anapo
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
  # 1 - Data design ----
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
  check_anapo_inspector_dataplot_range_date <- dplyr::inner_join(check_anapo_inspector_dataplot_range_date, dataframe2[, c("activity_date", "trip_id", "activity_id")], by = dplyr::join_by(date_group == activity_date, trip_id == trip_id))
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

# Function to create the plot of the consistency of the position for the activity and VMS
plot_anapo <- function(data_vms, crs_vms, crs_activity, data_activity, data_trip) {
  # Local binding global variables
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
  # Plot
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

# Function to create the windows with plot of the consistency of the position for the activity and VMS
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

# Function to create list data/argument for the plot plot_anapo_activity
plot_anapo_activity_data <- function(dataframe1, dataframe2, dataframe3) {
  # 0 - Global variables assignement ----
  vms_id <- NULL
  # 1 - Data design ----
  # Retrieving information for the plot
  check_anapo_activity_dataplot <- dplyr::inner_join(dataframe1, dataframe2[, c("vms_id", "vms_time", "vms_position", "vms_crs")], by = dplyr::join_by(vms_id))
  check_anapo_activity_dataplot <- data_to_list(data = check_anapo_activity_dataplot, name_col_dataplot = "data_vms", colname_id = "vms_id", colname_plot = c("vms_position", "vms_time"), colname_info = c("vms_date", "vms_crs", "vessel_code", "vessel_type"), rename_colname_info = c("date_vms", "crs_vms", "vessel_code", "vessel_type"))
  return(check_anapo_activity_dataplot)
}

# Function to create the plot of the consistency of the position for VMS
plot_anapo_activity <- function(data_vms, crs_vms, date_vms) {
  # Local binding global variables
  . <- NULL
  vms_time <- NULL
  vms_time_bis <- NULL
  date_time <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
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
  # Plot
  plot <- plotly::plot_ly() %>%
    plotly::layout(mapbox = list(style = "carto-positron", pitch = 0, zoom = 6))
  if (!all(is.na(data_vms$vms_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "VMS", data = data_geo_vms, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", hovertemplate = ~text, marker = list(color = grDevices::colorRampPalette(c("#00F7FF", "#3B18AA"))(nrow(data_geo_vms)), size = 10), line = list(color = "#0032FF")) %>%
      plotly::layout(showlegend = TRUE, mapbox = list(center = list(lon = data_geo_vms$X[1], lat = data_geo_vms$Y[1])))
  }
  return(plot)
}

# Function to create the windows with plot of the consistency of the position for VMS
plot_anapo_activity_windows <- function(vessel_code, date_vms, vessel_type) {
  paste0("<b>Trip information : </b><br>
         <ul><li>Vessel code : ", vessel_code, "</li>
         <li>VMS date : ", date_vms, "</li>
         <li>Vessel type : ", vessel_type, "</li></ul>")
}


# Function detects activity linked solely to grounding
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

# Function to list elements with a number of occurrences other than 2
text_object_more_or_less <- function(id, result_check) {
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

# Function for converting DD (Decimal Degrees) coordinates in DDM (Degrees, Decimal Minutes)
coordinate_dd_to_dmd <- function(coordinate, latitude) {
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
