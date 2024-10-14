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
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  activity_id}}
#' }
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id")]
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  route_fishingtime}}
#' }
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  dataframe1 <- merge(dataframe1, dataframe2, by = "trip_id", all.x = TRUE)
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  route_seatime}}
#' }
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  dataframe1 <- merge(dataframe1, dataframe2, by = "trip_id", all.x = TRUE)
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  trip_id}}
#' }
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  dataframe1 <- merge(dataframe1, dataframe2, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  dataframe1$difference <- ifelse(is.na(dataframe1$trip_landingtotalweight), 0, dataframe1$trip_landingtotalweight) - ifelse(is.na(dataframe1$sum_weightlanding), 0, dataframe1$sum_weightlanding)
  dataframe1$difference <- abs(dataframe1$difference)
  dataframe1$epsilon <- epsilon
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  trip_id}}
#' }
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge date
  dataframe1 <- merge(dataframe1, dataframe2, by = "trip_id", all.x = TRUE)
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
    dplyr::summarise(count_freq = length(activity_date), .groups = "keep")
  # Calculation if an inconsistency among the different tests on the date has been found
  trip_date_activity_data_detail$logical <- trip_date_activity_data_detail$inter_activity_date & !trip_date_activity_data_detail$exter_activity_date & trip_date_activity_data_detail$count_freq == 1
  # Calculation if the number of days is consistent and if there are inconsistencies in the dates for the trips
  dataframe1 <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(nb_day = length(activity_date), logical_tmp = sum(!logical) == 0, .groups = "keep")
  # Calculation if an inconsistency among the different tests on the trip has been found
  dataframe1 <- dataframe1 %>%
    dplyr::summarise(logical = sum(c(!((1 + trip_enddate - trip_startdate) == nb_day), !logical_tmp)) == 0, .groups = "keep")
  # Management of missing trip start and end date
  dataframe1[is.na(dataframe1$trip_startdate) | is.na(dataframe1$trip_enddate), "logical"] <- FALSE
  dataframe1 <- subset(dataframe1, select = -c(trip_startdate, trip_enddate))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @export
check_harbour_inspector <- function(dataframe1,
                                    output) {
  # 0 - Global variables assignement ----
  harbour_label_departure <- NULL
  harbour_label_landing_trip_previous <- NULL
  harbour_id_departure <- NULL
  harbour_id_landing_trip_previous <- NULL
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
      output = "message"
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
      output = "message"
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
  # Management of missing harbour
  dataframe1[is.na(dataframe1$harbour_id_landing_trip_previous), "logical"] <- FALSE
  dataframe1[is.na(dataframe1$harbour_id_departure) & !is.na(dataframe1$trip_previous_id), "logical"] <- FALSE
  # Management of not trip previous
  dataframe1[is.na(dataframe1$trip_previous_id), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, harbour_label_departure, harbour_label_landing_trip_previous, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(harbour_id_departure, harbour_id_landing_trip_previous, trip_previous_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @param speciesfate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to calculate catch weight in RF1.
#' @param vesselactivity {\link[base]{character}} expected. Default values: c("23", "25", "27", "29"). Vector of inventory of vessel activity NOT used to calculate catch weight in RF1.
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.9 and 1.1. Vector containing the lower and upper acceptable threshold for RF1.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  trip_id}}
#' Dataframe 3:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  trip_id}}
#' Dataframe 4:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_end_tide_id}}
#'  \item{\code{  vessel_id}}
#'  \item{\code{  country_fleetcountry}}
#' }
#' @export
check_raising_factor_inspector <- function(dataframe1,
                                           dataframe2,
                                           dataframe3,
                                           dataframe4,
                                           output,
                                           country_species = list("1" = c("TUN", "ALB", "YFT", "BET", "SKJ"), "4" = c("LOT", "TUN", "ALB", "YFT", "BET", "SKJ", "LTA", "FRI", "BLF", "RAV*", "KAW", "FRZ", "BLT")),
                                           speciesfate = "6",
                                           vesselactivity = c("23", "25", "27", "29"),
                                           threshold = c(0.9, 1.1)) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  catch_weight <- NULL
  trip_landingtotalweight <- NULL
  sum_catch_weight <- NULL
  rf1 <- NULL
  tide_sum_landing_weight <- NULL
  tide_sum_catch_weight <- NULL
  lower_threshold <- NULL
  upper_threshold <- NULL
  species_fao_code <- NULL
  speciesfate_code <- NULL
  vesselactivity_code <- NULL
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id")]
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
      output = "message"
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
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("landing_id", "landing_weight", "species_fao_code", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("trip_id", "trip_end_tide_id", "vessel_id", "country_fleetcountry"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("trip_id", "trip_end_tide_id", "vessel_id", "country_fleetcountry"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe4 <- dataframe4[, c("trip_id", "trip_end_tide_id", "vessel_id", "country_fleetcountry")]
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
      output = "message"
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
      output = "message"
    ))
  }
  # Checks the type of speciesfate
  if (!codama::r_type_checking(
    r_object = speciesfate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = speciesfate,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of vesselactivity
  if (!codama::r_type_checking(
    r_object = vesselactivity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vesselactivity,
      type = "character",
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Add country_fleetcountry for catch
  dataframe2 <- merge(dataframe2, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = "trip_id", all.x = TRUE)
  # Catch filtration for RF1
  ## Selection species when the list is available for the country and selection species
  condition<-as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe2_select_species <- purrr::map(condition, ~ dataframe2 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe2_select_species <- do.call(rbind.data.frame, dataframe2_select_species)
  ## Selection all species when the list is not available for the country
  dataframe2<- rbind(dataframe2_select_species ,dataframe2 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  ## Selection species fate
  dataframe2 <- dataframe2 %>%
    dplyr::filter((speciesfate_code %in% speciesfate))
  ## Selection vessel activity
   dataframe2 <- dataframe2 %>%
     dplyr::filter(!(vesselactivity_code %in% vesselactivity))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- merge(dataframe4, dataframe2, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Add country_fleetcountry for landing
  dataframe3 <- merge(dataframe3, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = "trip_id", all.x = TRUE)
  # Landing filtration for RF1
  ## Selection species when the list is available for the country and selection species fate
  condition<-as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe3_select_species <- purrr::map(condition, ~ dataframe3 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe3_select_species <- do.call(rbind.data.frame, dataframe3_select_species)
  ## Selection all species when the list is not available for the country
  dataframe3<- rbind(dataframe3_select_species ,dataframe3 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_landing_weight = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- merge(dataframe4, dataframe3, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Add of a logic that indicates whether the tide is finished or not
  dataframe4$logical_full_tide <- !is.na(dataframe4$trip_end_tide_id)
  # For unfinished tides (no end-of-tide id) indicates the vessel id for the end-of-tide id (for each ship, allows you to group together all the trips of the non-finished tide)
  dataframe4[is.na(dataframe4$trip_end_tide_id),"trip_end_tide_id"] <- paste0("vessel_id_", dataframe4[is.na(dataframe4$trip_end_tide_id),"vessel_id", drop = TRUE])
  # RF1 calculation
  tide_id_data_rf1 <- dataframe4 %>%
    dplyr::group_by(trip_end_tide_id) %>%
    dplyr::summarise(rf1 = ifelse(all(is.na(sum_landing_weight)), sum_landing_weight[NA_integer_], sum(sum_landing_weight, na.rm = TRUE)) / ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)), tide_sum_landing_weight = ifelse(all(is.na(sum_landing_weight)), sum_landing_weight[NA_integer_], sum(sum_landing_weight, na.rm = TRUE)), tide_sum_catch_weight = ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)))
  dataframe4$lower_threshold <- threshold[1]
  dataframe4$upper_threshold <- threshold[2]
  # Selection of user-supplied trips
  dataframe4 <- merge(data.frame(trip_id = dataframe1$trip_id), unique(dataframe4), by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Merge data
  dataframe4 <- merge(dataframe4, tide_id_data_rf1, by.x = "trip_end_tide_id", by.y = "trip_end_tide_id", all.x = TRUE)
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
  dataframe4[(is.na(dataframe4$tide_sum_landing_weight) | dataframe4$tide_sum_landing_weight == 0) & is.na(dataframe4$tide_sum_catch_weight), "logical"] <- TRUE
  dataframe4 <- dplyr::relocate(.data = dataframe4, rf1, .after = logical)
  dataframe4 <- subset(dataframe4, select = -c(trip_end_tide_id, logical_full_tide, sum_catch_weight, sum_landing_weight, tide_sum_landing_weight, tide_sum_catch_weight, lower_threshold, upper_threshold, vessel_id, country_fleetcountry))
  if ((sum(dataframe4$logical, na.rm = TRUE) + sum(!dataframe4$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe4$logical)) > 0) {
    all <- c(select, dataframe4$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe4$logical)) > 0) {
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
#' @param schooltype_object {\link[base]{character}} expected, default : c("1"). Vector of inventory of code of the object school.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' Dataframe 2:
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' }
#' @export
check_fishing_context_inspector <- function(dataframe1,
                                            dataframe2,
                                            output,
                                            schooltype_object = c("1")) {
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = schooltype_object,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = schooltype_object,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Filters out object-school associations
  dataframe2 <- dataframe2 %>% dplyr::filter(schooltype_code %in% schooltype_object)
  # Calculates the number of object-type associations
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(association_object_count = dplyr::n())
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2, all.x = TRUE)
  dataframe1$threshold <- 0
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
  dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code == "2"] <- !dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code == "2"]
  # Unknown benches and NA: no constraint
  dataframe1$logical[is.na(dataframe1$schooltype_code) | dataframe1$schooltype_code == "0"] <- TRUE
  dataframe1 <- dplyr::relocate(.data = dataframe1, schooltype_code, association_object_count, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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

#' @name check_operationt_inspector
#' @title Gives the inconsistencies between fishing success status, vessel activity, type of school or weight caught
#' @description The purpose of the check_operationt_inspector function is to provide a table of data that contains an inconsistency with succes status and vessel activity, the type of school or the weight caught
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_operationt_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
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
#' @export
check_operationt_inspector <- function(dataframe1,
                                       output) {
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Indicates whether the vessel activity requires a success status
  dataframe1$threshold <- "6"
  comparison_successstatus_vesselactivity <- codama::vector_comparison(
    first_vector = dataframe1$vesselactivity_code,
    second_vector = dataframe1$threshold,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical_successstatus_vesselactivity <- comparison_successstatus_vesselactivity$logical
  # Case of success status NA: must not have activity 6 (inverse of the result obtained)
  dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)] <- !dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)]
  # Indicates indeterminate school must not have positive or negative success status
  dataframe1$threshold <- "0"
  logical_successstatus_schooltype_indeterminate <- codama::vector_comparison(
    first_vector = dataframe1$schooltype_code,
    second_vector = dataframe1$threshold,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_successstatus_schooltype_indeterminate <- !logical_successstatus_schooltype_indeterminate$logical
  # Case of success status indeterminate or NA: no constraints
  dataframe1$logical_successstatus_schooltype_indeterminate[is.na(dataframe1$successstatus_code) | dataframe1$successstatus_code == "2"] <- TRUE
  # Indicates whether the success status requires a school type
  dataframe1$logical_successstatus_schooltype <- !is.na(dataframe1$schooltype_code)
  # Case of success status NA: no constraints
  dataframe1$logical_successstatus_schooltype[is.na(dataframe1$successstatus_code)] <- TRUE
  # Indicates whether captured weight is greater than 0
  dataframe1$threshold <- 0
  comparison_successstatus_weight <- codama::vector_comparison(
    first_vector = dataframe1$activity_weight,
    second_vector = dataframe1$threshold,
    comparison_type = "greater",
    output = "report"
  )
  dataframe1$logical_successstatus_weight <- comparison_successstatus_weight$logical
  # Case of success status null : must not have weight (inverse of the result obtained)
  dataframe1$logical_successstatus_weight[!is.na(dataframe1$successstatus_code) & dataframe1$successstatus_code == "0"] <- !dataframe1$logical_successstatus_weight[!is.na(dataframe1$successstatus_code) & dataframe1$successstatus_code == "0"]
  # NA success status: no constraints
  dataframe1$logical_successstatus_weight[is.na(dataframe1$successstatus_code)] <- TRUE
  # Combines test results
  dataframe1$logical <- dataframe1$logical_successstatus_vesselactivity & dataframe1$logical_successstatus_schooltype_indeterminate & dataframe1$logical_successstatus_schooltype & dataframe1$logical_successstatus_weight
  dataframe1 <- dplyr::relocate(.data = dataframe1, vesselactivity_code, successstatus_code, schooltype_code, activity_weight, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold, logical_successstatus_vesselactivity, logical_successstatus_schooltype_indeterminate, logical_successstatus_schooltype, logical_successstatus_weight))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @param path_shp_sea {\link[base]{character}} expected. Path to the folder containing the seas shapefile
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param harbour_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position harbour
#' @param layer_shp_sea {\link[base]{character}} expected. Default values: World_Seas. Layer to containing the seas shapefile (cf : Flanders Marine Institute. IHO Sea Areas, version 1. Available online at https://www.marineregions.org/)
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
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_position_departure}}
#'  \item{\code{  harbour_position_landing}}
#' }
#' @export
check_position_inspector <- function(dataframe1,
                                     dataframe2,
                                     path_shp_sea,
                                     output,
                                     activity_crs = 4326,
                                     harbour_crs = 4326,
                                     layer_shp_sea = "World_Seas",
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
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "ocean_label", "activity_position", "activity_crs", "trip_id"),
    column_type = c("character", "character", "character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "ocean_label", "activity_position", "activity_crs", "trip_id"),
      column_type = c("character", "character", "character", "numeric", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "ocean_label", "activity_position", "activity_crs", "trip_id")]
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
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "harbour_position_departure", "harbour_position_landing")]
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = path_shp_sea,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = path_shp_sea,
      type = "character",
      length = 1L,
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = layer_shp_sea,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = layer_shp_sea,
      type = "character",
      length = 1L,
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(select)
  # 2 - Data design ----
  dataframe1 <- merge(dataframe1, dataframe2, by = "trip_id", all.x = TRUE)
  # Indicates whether in land harbour
  dataframe1$logical_harbour <- FALSE
  # Formats spatial data activity
  data_geo_activity <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(., wkt = "activity_position", crs = activity_crs) %>%
    sf::st_transform(activity_position, crs = 4326)
  # If harbour departure position exists
  if (sum(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_departure)) > 0) {
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
    dataframe1 <- merge(dataframe1, data.frame(data_geo_activity_harbourdeparture)[, c("activity_id", "logical_harbourdeparture")], by = "activity_id", all.x = TRUE)
    dataframe1$logical_harbourdeparture[is.na(dataframe1$logical_harbourdeparture)] <- FALSE
  }else {
    dataframe1$logical_harbourdeparture <- FALSE
  }
  # If harbour landing position exists
  if (sum(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_landing)) > 0) {
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
    dataframe1 <- merge(dataframe1, data.frame(data_geo_activity_harbourlanding)[, c("activity_id", "logical_harbourlanding")], by = "activity_id", all.x = TRUE)
    dataframe1$logical_harbourlanding[is.na(dataframe1$logical_harbourlanding)] <- FALSE
  }else {
    dataframe1$logical_harbourlanding <- FALSE
  }
    # Logical in harbour
    dataframe1$logical_harbour <- dataframe1$logical_harbour | dataframe1$logical_harbourdeparture | dataframe1$logical_harbourlanding
   # Reading the file with sea shapes
  shape_sea <- sf::read_sf(dsn =  path_shp_sea, layer = layer_shp_sea)
  # Grouping of sea pieces
  id_atlantic <- c("15", "15A", "21", "21A", "22", "23", "26", "27", "32", "34")
  id_indian <- c("45", "45A", "44", "46A", "62", "42", "43", "38", "39")
  # Buffer in degrees
  shape_sea_atlantic <- shape_sea[shape_sea$ID %in% id_atlantic, ] %>%
    terra::vect() %>%
    terra::buffer(width = buffer_sea) %>%
    sf::st_as_sf()
  shape_sea_indian <- shape_sea[shape_sea$ID %in% id_indian, ] %>%
    terra::vect() %>%
    terra::buffer(width = buffer_sea) %>%
    sf::st_as_sf()
  # Calculates the intersection between activity and sea
  intersect_atlantic <- sf::st_intersects(data_geo_activity, shape_sea_atlantic)
  intersect_indian <- sf::st_intersects(data_geo_activity, shape_sea_indian)
  # Logical in sea, indicates whether the ocean is the same
  data_geo_activity$logical_ocean <- FALSE
  data_geo_activity$logical_ocean <- (lengths(intersect_atlantic) != 0 & data_geo_activity$ocean_label == "Atlantic") | (lengths(intersect_indian) != 0 & data_geo_activity$ocean_label == "Indian")
  data_geo_activity$ocean_calculate <- NA
  data_geo_activity$ocean_calculate[lengths(intersect_atlantic) != 0] <- "Atlantic"
  data_geo_activity$ocean_calculate[lengths(intersect_indian) != 0] <- "Indian"
  dataframe1 <- merge(dataframe1, data.frame(data_geo_activity)[, c("activity_id", "logical_ocean", "ocean_calculate")], by = "activity_id", all.x = TRUE)
  dataframe1$logical_ocean[is.na(dataframe1$logical_ocean)] <- FALSE
  # Case where the position is exactly on the boundary of the two oceans
  data_geo_activity$ocean_calculate[lengths(intersect_atlantic) != 0 & lengths(intersect_indian) != 0] <- data_geo_activity$ocean_label[lengths(intersect_atlantic) != 0 & lengths(intersect_indian) != 0]
  # Case of harbour in sea : not in harbour
  dataframe1$logical_harbour[!is.na(dataframe1$ocean_calculate)] <- FALSE
  dataframe1$logical <- dataframe1$logical_ocean | dataframe1$logical_harbour
  # Gives the type of location
  dataframe1$type <- "Sea"
  dataframe1$type[is.na(dataframe1$ocean_calculate)] <- "Excluding Atlantic Indian oceans"
  dataframe1$type[is.na(dataframe1$activity_position)] <- "No position"
  dataframe1$type[dataframe1$logical_harbour] <- "Harbour"
  dataframe1 <- dplyr::relocate(.data = dataframe1, type, ocean_label, ocean_calculate, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(trip_id, harbour_position_departure, harbour_position_landing, logical_harbourdeparture, logical_harbourlanding))
  activity_sea_land_data_detail <- dataframe1
  dataframe1 <- subset(dataframe1, select = -c(activity_position, activity_crs, logical_ocean, logical_harbour))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_weight}}
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  activity_id}}
#' }
#' @export
check_weight_inspector <- function(dataframe1,
                                   dataframe2,
                                   output) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  catch_weight <- NULL
  activity_weight <- NULL
  sum_catch_weight <- NULL
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the sum of the weight per activity (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Group the pair to compare
  dataframe1 <- merge(dataframe1, dataframe2, by = "activity_id", all.x = TRUE)
  # Compare weight of the activity or the sum of the catch
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$activity_weight,
    second_vector = dataframe1$sum_catch_weight,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, activity_weight, sum_catch_weight, .after = logical)
  # Management of the NA value for the weight activity and catch
  dataframe1[is.na(dataframe1$activity_weight) & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  # Management of the 0 value for the weight activity
  dataframe1[!is.na(dataframe1$activity_weight) & dataframe1$activity_weight == 0 & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @param threshold {\link[base]{numeric}} expected. Default values: 80. Threshold of the size measure
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$samplespeciesmeasure_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  dataframe1$threshold <- threshold
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$samplespeciesmeasure_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  sample_id}}
#' }
#' @export
check_measure_inspector <- function(dataframe1,
                                    dataframe2,
                                    output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "samplespecies_measuredcount", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "sample_id")]
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
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculates the total sum of individuals by sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_measuredcount = ifelse(all(is.na(samplespecies_measuredcount)), samplespecies_measuredcount[NA_integer_], sum(samplespecies_measuredcount, na.rm = TRUE)))
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_count = ifelse(all(is.na(samplespeciesmeasure_count)), samplespeciesmeasure_count[NA_integer_], sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)
  # Compare the two sums
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$sum_measuredcount,
    second_vector = dataframe1$sum_count,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, sum_measuredcount, sum_count, .after = logical)
  # Management of missing count measurements by sample and by species
  dataframe1[is.na(dataframe1$sum_measuredcount), "logical"] <- FALSE
  # Management of missing count measurements by sample and by species and by size class
  dataframe1[is.na(dataframe1$sum_count), "logical"] <- FALSE
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare RF1 to valid threshold
  dataframe1$lower_threshold <- threshold[1]
  dataframe1$upper_threshold <- threshold[2]
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). list of the inventory of species (FAO code) used to compare to sample weighting.
#' @param speciesfate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to compare to sample weighting.
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  activity_id}}
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @export
check_weighting_sample_inspector <- function(dataframe1,
                                      dataframe2,
                                      output,
                                      species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"),
                                      speciesfate = "6",
                                      epsilon = 0.01) {
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
  sum_landing_weight <- NULL
  weight <- NULL
  vesseltype_code <- NULL
  weightedweight_bis <- NULL
  sum_landing_weight_bis <- NULL
  sampletype_code <- NULL
  vesseltype_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
    column_type = c("character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
      column_type = c("character", "numeric", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sampleactivity_weightedweight", "activity_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "sampleactivity_weightedweight", "activity_id"),
      column_type = c("character", "numeric", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "sampleactivity_weightedweight", "activity_id")]
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = speciesfate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = speciesfate,
      type = "character",
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe1 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::filter(species_fao_code %in% species & speciesfate_code %in% speciesfate) %>%
    dplyr::summarise(weight = ifelse(all(is.na(catch_weight)), 0, sum(catch_weight, na.rm = TRUE)))
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)))
  # Merge
  dataframe1 <- dataframe1 %>%
      dplyr::select(activity_id) %>%
      dplyr::distinct()
  dataframe1 <- merge(dataframe1, dataframe_weight_calculation, by = "activity_id", all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe2, by = "activity_id", all.x = TRUE)
  # Calcul difference
  dataframe1$difference <- ifelse(is.na(dataframe1$weight), 0, dataframe1$weight) - ifelse(is.na(dataframe1$weightedweight), 0, dataframe1$weightedweight)
  dataframe1$difference <- abs(dataframe1$difference)
  dataframe1$epsilon <- epsilon
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespeciesmeasure_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id")]
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id")]
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespecies_subsamplenumber}}
#'  \item{\code{  sample_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search subsample number in the associations samples ID
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count_subsamplenumber_n0 = sum(samplespecies_subsamplenumber != 0), count_subsamplenumber_0 = sum(samplespecies_subsamplenumber == 0), count_samplespecies = sum(!is.na(unique(samplespecies_id))), count_subsamplenumber_1 = sum(samplespecies_subsamplenumber == 1))
  # Merge
  dataframe1$logical <- TRUE
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  # Case of NA count_subsamplenumber_n0, count_subsamplenumber_0, count_samplespecies or count_subsamplenumber_1
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      count_subsamplenumber_n0_bis = dplyr::coalesce(count_subsamplenumber_n0, 0),
      count_subsamplenumber_0_bis = dplyr::coalesce(count_subsamplenumber_0, 0),
      count_samplespecies_bis = dplyr::coalesce(count_samplespecies, 0),
      count_subsamplenumber_1_bis = dplyr::coalesce(count_subsamplenumber_1, 0),
    )
  dataframe1[dataframe1$count_samplespecies_bis == 0, "logical"] <- FALSE
  dataframe1$only_one_subsampling <- dataframe1$sample_supersample == FALSE & dataframe1$count_subsamplenumber_n0_bis == 0
  dataframe1$many_subsampling <- dataframe1$sample_supersample == TRUE & dataframe1$count_subsamplenumber_0_bis == 0 & dataframe1$count_samplespecies_bis > 1
  dataframe1[!(dataframe1$only_one_subsampling | dataframe1$many_subsampling), "logical"] <- FALSE
  dataframe1[dataframe1$count_samplespecies_bis == 1 & dataframe1$count_subsamplenumber_1_bis > 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(only_one_subsampling, many_subsampling, count_samplespecies_bis, count_subsamplenumber_n0_bis, count_subsamplenumber_0_bis, count_subsamplenumber_1_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_supersample, count_subsamplenumber_n0, count_subsamplenumber_0, count_subsamplenumber_1, count_samplespecies, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  trip_id}}
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  well_well}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @export
check_well_number_consistent_inspector <- function(dataframe1,
                                                   dataframe2,
                                                   output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  sample_well <- NULL
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "well_label"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "well_label"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "well_label")]
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
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # merge
  dataframe2$logical <- TRUE
  dataframe1 <- merge(dataframe1, dataframe2, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_label"), all.x = TRUE)
  # Search well not link
  dataframe1[is.na(dataframe1$logical), "logical"] <- FALSE
  # Case the well number is empty
  dataframe1[is.na(dataframe1$sample_well), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_well, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  sample_id}}
#' Dataframe 3:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      column_type = c("character", "numeric", "numeric", "numeric"), output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe3, by = "samplespecies_id", all.x = TRUE)
  # Calculate the number of the measure per sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  total_count <- dataframe1 %>%
    dplyr::group_by(sample_id, sample_smallsweight, sample_bigsweight, sample_totalweight) %>%
    dplyr::summarise(total_count = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)), .groups = "keep")
  # Small and large category conditions
  condition <- as.list(as.data.frame(t(data.frame(species_big, size_measure_type_big, threshold_size_big))))
  # Measurement selection of small individuals
  little <- purrr::map(condition, ~ dataframe1 %>%
                         dplyr::filter(species_fao_code == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass < as.numeric(.x[3])))
  little <- do.call(rbind.data.frame, little)
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
  big <- do.call(rbind.data.frame, big)
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
  total_count <- merge(total_count, little, by = "sample_id", all.x = TRUE)
  total_count <- merge(total_count, big, by = "sample_id", all.x = TRUE)
  total_count <- merge(total_count, measure1, by = "sample_id", all.x = TRUE)
  total_count <- merge(total_count, measure2, by = "sample_id", all.x = TRUE)
  # Calculates percentages
  total_count <- total_count %>%
    dplyr::mutate(little_percentage = little / total_count, big_percentage = big / total_count, measure1_percentage = measure1 / total_count, measure2_percentage = measure2 / total_count)
  # Case of NA sample_smallsweight, sample_bigsweight or sample_totalweight
  total_count <- total_count %>%
    dplyr::mutate(
      sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
      sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
      sample_totalweight_bis = dplyr::coalesce(sample_totalweight, 0)
    )
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
  total_count <- dplyr::relocate(.data = total_count, sample_smallsweight, sample_bigsweight, sample_totalweight, little_percentage, big_percentage, measure1_percentage, measure2_percentage, .after = logical)
  if ((sum(total_count$logical, na.rm = TRUE) + sum(!total_count$logical, na.rm = TRUE)) != nrow_first || sum(is.na(total_count$logical)) > 0) {
    all <- c(select, total_count$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(total_count$logical)) > 0) {
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
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.95. Percentage threshold between weight and weighted weight for seiners
#' @param sampletype_code_landing_baitboat {\link[base]{character}} expected. Default values: c("11"). List of sample type codes for baitboat fresh landings
#' @param landingtype_baitboat {\link[base]{character}} expected. Default values: c("L-YFT-10", "L-BET-10", "L-TUN-10"). List of codes for fresh baitboat landings
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
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#' Dataframe 3:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vesseltype_code}}
#'  \item{\code{  vesseltype_label1}}
#' Dataframe 4:
#'  \item{\code{  trip_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  weightcategory_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @export
check_weighting_inspector <- function(dataframe1,
                                      dataframe2,
                                      dataframe3,
                                      dataframe4,
                                      output,
                                      vessel_type = c("6", "2"),
                                      threshold_weight = 100,
                                      threshold = 0.95,
                                      sampletype_code_landing_baitboat = c("11"),
                                      landingtype_baitboat = c("L-YFT-10", "L-BET-10", "L-TUN-10")) {
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
  sum_landing_weight <- NULL
  weight <- NULL
  vesseltype_code <- NULL
  weightedweight_bis <- NULL
  sum_landing_weight_bis <- NULL
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sampleactivity_weightedweight"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "sampleactivity_weightedweight"),
      column_type = c("character", "numeric"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "sampleactivity_weightedweight")]
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
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code", "vesseltype_label")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("trip_id", "landing_weight", "weightcategory_code"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("trip_id", "landing_weight", "weightcategory_code"),
      column_type = c("character", "numeric", "character"),
      output = "message"
    )
  } else {
    dataframe4 <- dataframe4[, c("trip_id", "landing_weight", "weightcategory_code")]
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = landingtype_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = landingtype_baitboat,
      type = "character",
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = sampletype_code_landing_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = sampletype_code_landing_baitboat,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), 0, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE)))
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight = ifelse(is.na(sample_totalweight) | sample_totalweight == 0, weight_calculation, sample_totalweight))
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)))
  # Calculation fresh landing baitboat (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe4 <- dataframe4 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::filter(weightcategory_code %in% landingtype_baitboat) %>%
    dplyr::summarise(sum_landing_weight = ifelse(all(is.na(landing_weight)), 0, sum(landing_weight, na.rm = TRUE)))
  # Merge
  dataframe1$logical <- TRUE
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe3, by = "trip_id", all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe4, by = "trip_id", all.x = TRUE)
  # Case of NA weightedweight or sum_landing_weight
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      weightedweight_bis = dplyr::coalesce(weightedweight, 0),
      sum_landing_weight_bis = dplyr::coalesce(sum_landing_weight, 0)
    )
  # Check
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weight > threshold_weight, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weightedweight_bis < dataframe1$weight & !((dataframe1$weightedweight_bis / dataframe1$weight) >= threshold), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & dataframe1$sampletype_code %in% sampletype_code_landing_baitboat & abs(dataframe1$weightedweight_bis - dataframe1$sum_landing_weight_bis) > 1, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & !(dataframe1$sampletype_code %in% sampletype_code_landing_baitboat) & abs(dataframe1$weightedweight_bis - dataframe1$weight) > 1, "logical"] <- FALSE
  # Case NA vesseltype_code sampletype_code
  dataframe1[is.na(dataframe1$vesseltype_code), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & is.na(dataframe1$sampletype_code), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_calculation, weight, vesseltype_code, weightedweight_bis, sum_landing_weight_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_totalweight, sampletype_code, weightedweight, vesseltype_label, sum_landing_weight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), NaN, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE)))
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
  dataframe1$logical <- !(comparison_weight_calculation$logical & comparison_totalweight$logical) & !(is.na(dataframe1$weight_calculation) & is.na(dataframe1$sample_totalweight))
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(weight_calculation))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_totalweight, sample_smallsweight, sample_bigsweight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id")]
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
      output = "message"
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
      output = "message"
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
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
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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

#' @name check_distribution_inspector
#' @title Gives the inconsistencies between the weights of small and big sample fish and the sum of the small and big weights in the associated well
#' @description The purpose of the check_distribution_inspector  function is to provide a table of data that contains an inconsistency between the small and large sample weights and the sum of the small and big weights of the associated well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("SKJ"). Vector of species categorized as small if weight category information is missing
#' @param weightcategory_small {\link[base]{character}} expected. Default values: c("W-1"). Vector of small weight category codes
#' @param weightcategory_big {\link[base]{character}} expected. Default values: c("W-2"). Vector of big weight category codes
#' @param weightcategory_unknown {\link[base]{character}} expected. Default values: c("W-9"). Vector of unknown weight category codes
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#' Dataframe 2:
#'  \item{\code{  well_id}}
#'  \item{\code{  well_well}}
#'  \item{\code{  trip_id}}
#' Dataframe 3:
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  well_id}}
#' Dataframe 4:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  weightcategory_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  wellactivityspecies_weight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @export
check_distribution_inspector <- function(dataframe1,
                                         dataframe2,
                                         dataframe3,
                                         dataframe4,
                                         output,
                                         species = c("SKJ"),
                                         weightcategory_small = c("W-1"),
                                         weightcategory_big = c("W-2"),
                                         weightcategory_unknown = c("W-9")) {
  # 0 - Global variables assignement ----
  well_id <- NULL
  trip_id <- NULL
  well_label <- NULL
  weightcategory_code <- NULL
  wellactivityspecies_weight <- NULL
  species_fao_code <- NULL
  sample_id <- NULL
  weight_small <- NULL
  weight_small_unknown <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  weight_big <- NULL
  weight_small_total <- NULL
  sample_smallsweight_bis <- NULL
  sample_bigsweight_bis <- NULL
  weight_small_total_bis <- NULL
  weight_big_bis <- NULL
  sample_well <- NULL
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weightcategory_small,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weightcategory_small,
      type = "character",
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weightcategory_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weightcategory_big,
      type = "character",
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weightcategory_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weightcategory_unknown,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe2 <- merge(dataframe2, dataframe3, by = "well_id", all.x = TRUE)
  dataframe2 <- merge(dataframe2, dataframe4, by = "wellactivity_id", all.x = TRUE)
  # Calculation small weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2_small <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_label) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_small) %>%
    dplyr::reframe(weight_small = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  dataframe2_small_unknown <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_label) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_unknown & species_fao_code %in% species) %>%
    dplyr::reframe(weight_small_unknown = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  # Calculation big weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2_big <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_label) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_big) %>%
    dplyr::reframe(weight_big = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2_small, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_label"), all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe2_small_unknown, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_label"), all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe2_big, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_label"), all.x = TRUE)
  # Calculation small weight total (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_small_total = ifelse(all(is.na(c(weight_small, weight_small_unknown))), NaN, sum(c(weight_small, weight_small_unknown), na.rm = TRUE)))
  # Case of NA
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
      sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
      weight_big_bis = dplyr::coalesce(weight_big, 0),
      weight_small_total_bis = dplyr::coalesce(weight_small_total, 0)
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
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_well, weight_small_total, weight_big, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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

#' @name check_anapo_inspector
#' @title Gives the inconsistencies activity position and VMS position
#' @description The purpose of the check_anapo_inspector function is to provide a table of data that contains an inconsistency between activity position and VMS position
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_anapo_inspector () function.
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param vms_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position VMS
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param minimum_number_vms {\link[base]{numeric}} expected. Default values: 20. Minimum number of VMS positions required.
#' @param threshold {\link[base]{numeric}} expected. Default values: 10. Maximum valid distance threshold (Nautical miles) between position and nearest VMS point.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  activity_crs}}
#'  \item{\code{  vessel_code}}
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  harbour_id}}
#' Dataframe 3:
#'  \item{\code{  vms_date}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  vms_crs}}
#'  \item{\code{  vessel_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @export
check_anapo_inspector <- function(dataframe1,
                                  dataframe2,
                                  dataframe3,
                                  activity_crs = 4326,
                                  vms_crs = 4326,
                                  output,
                                  minimum_number_vms = 20,
                                  threshold = 10) {
  # 0 - Global variables assignement ----
  vms_date <- NULL
  vessel_code <- NULL
  nb_vms <- NULL
  activity_position_geom <- NULL
  vms_position_geom <- NULL
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
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "vessel_code"),
    column_type = c("character", "Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "vessel_code"),
      column_type = c("character", "Date", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_date", "activity_time", "activity_position", "vessel_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "harbour_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "harbour_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "harbour_id")]
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
      output = "message"
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
      output = "message"
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
      output = "message"
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
      output = "message"
    ))
  }
  if (!codama::r_type_checking(
    r_object = minimum_number_vms,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = minimum_number_vms,
      type = "numeric",
      length = 1L,
      output = "message"
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
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Remove VMS without position
  dataframe3 <- dataframe3 %>% dplyr::filter(!is.na(vms_position))
  # Calculation number vms
  dataframe3_nb_vms <- dataframe3 %>%
    dplyr::group_by(vms_date, vessel_code) %>%
    dplyr::summarise(nb_vms = dplyr::n(), .groups = "keep")
  # Merge
  dataframe1$logical <- FALSE
  dataframe1 <- merge(dataframe1, dataframe3_nb_vms, by.x = c("activity_date", "vessel_code"), by.y = c("vms_date", "vessel_code"), all.x = TRUE)
  # Case of NA nb_vms
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      nb_vms_bis = dplyr::coalesce(nb_vms, 0),
    )
  # Indicates activity whether in harbour
  dataframe2 <- dataframe2[!is.na(dataframe2$harbour_id), ]
  comparison_harbour <- codama::vector_comparison(
    first_vector = dataframe1$activity_id,
    second_vector = dataframe2$activity_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1[comparison_harbour$logical, "logical"] <- TRUE
  # Retrieves VMS positions for the previous, current and next day
  dataframe3$date_group <- dataframe3$vms_date
  dataframe3_prior <- dataframe3 %>% dplyr::mutate(date_group = vms_date - 1)
  dataframe3_post <- dataframe3 %>% dplyr::mutate(date_group = vms_date + 1)
  dataframe3 <- rbind(dataframe3, dataframe3_prior, dataframe3_post) %>%
    dplyr::group_by(date_group, vms_position) %>%
    dplyr::distinct()
  dataframe3 <- merge(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position", "logical")], dataframe3, by.x = c("activity_date", "vessel_code"), by.y = c("date_group", "vessel_code"))
  # Formats spatial data
   dataframe_vms_geo <- dataframe3 %>%
      dplyr::filter(!logical & !is.na(activity_position)) %>%
      dplyr::select(vms_position) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(wkt = "vms_position", crs = vms_crs, remove = FALSE) %>%
      sf::st_transform(vms_position, crs = 4326)
  sf::st_geometry(dataframe_vms_geo) <- "vms_position_geometry"
  dataframe_activity_geo <- dataframe3 %>%
      dplyr::filter(!logical & !is.na(activity_position)) %>%
      dplyr::select(activity_position) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(wkt = "activity_position", crs = vms_crs, remove = FALSE) %>%
      sf::st_transform(activity_position, crs = 4326)
  sf::st_geometry(dataframe_activity_geo) <- "activity_position_geometry"
  # Select unique pair vms/activity
  pair_position <- dataframe3 %>%
      dplyr::filter(!logical & !is.na(activity_position)) %>%
      dplyr::select(vms_position, activity_position) %>%
      dplyr::distinct() %>%
      dplyr::mutate(pair_position = paste0(activity_position,"_", vms_position))
  pair_position <-merge(pair_position, dataframe_vms_geo, by ="vms_position", all.x = TRUE)
  pair_position <-merge(pair_position, dataframe_activity_geo, by ="activity_position", all.x = TRUE)
  # Calculation of the minimum distance between the activity and the nearest day's VMS in nautical mile
  # Define nautical miles
  units::install_unit("NM", "1852 m", "Nautical mile")
  threshold <- units::set_units(threshold, NM)
  pair_position <- pair_position %>%
      dplyr::mutate(distance = sf::st_distance(x = activity_position_geometry, y = vms_position_geometry, by_element = TRUE))
  units(pair_position$distance) <- units::make_units(NM)
  # Remove formats spatial data
  pair_position <- pair_position %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-c(activity_position_geometry, vms_position_geometry))
  dataframe3 <- merge(dataframe3, pair_position[, c("distance", "vms_position", "activity_position")], by = c("vms_position", "activity_position"), all.x = TRUE)
  rm(pair_position)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::mutate(min_distance = ifelse(length(distance) > 0, min(distance), Inf)) %>%
    dplyr::ungroup()
  units(dataframe3$min_distance) <- units::make_units(NM)
  dataframe_calcul_min <- dataframe3 %>%
    dplyr::select(activity_id, min_distance) %>%
    dplyr::distinct()
  dataframe1 <- merge(dataframe1, dataframe_calcul_min, by = "activity_id", all.x = TRUE)
  # Check if distance between activity and nearest VMS point below threshold
  dataframe1[!is.na(dataframe1$min_distance) & dataframe1$min_distance < threshold, "logical"] <- TRUE
  dataframe_calcul <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    subset(select=-c(logical)) %>%
    dplyr::filter(min_distance >= threshold)
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
    dplyr::mutate(score = (2^(units::drop_units(-distance / threshold))) * (2^(-units::drop_units(duration) / (2 * 60 * 60 * 1000))))
  dataframe_calcul[dataframe_calcul$distance > threshold * 2, "score"] <- 0
  dataframe_calcul[as.numeric(dataframe_calcul$duration) > (4 * 60 * 60 * 1000), "score"] <- 0
  dataframe_score_max <- dataframe_calcul %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(max_score = ifelse(length(score) > 0, max(score), -Inf))
  dataframe1 <- merge(dataframe1, dataframe_score_max, by = "activity_id", all.x = TRUE)
  # Check the maximum score between activity and VMS
  dataframe1[!is.na(dataframe1$max_score) & dataframe1$max_score >= 0.5, "logical"] <- TRUE
  # Check if the number of vms for the day exceeds the threshold
  dataframe1[dataframe1$nb_vms_bis < minimum_number_vms, "logical"] <- FALSE
  # Recovers all activity positions for the detailed table
  # Data with calcul VMS
  dataframe_detail <- dplyr::bind_rows(dataframe_calcul, dplyr::anti_join(subset(dataframe3, select=-c(logical)), dataframe_calcul, by = c("activity_id", "activity_date", "vms_date", "vessel_code", "vms_time", "vms_position", "activity_time", "activity_position")))
  dataframe_detail <- dplyr::bind_rows(dataframe_detail, dplyr::anti_join(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position")], dataframe3, by = c("activity_date" = "activity_date", "vessel_code" = "vessel_code")))
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(nb_vms_bis, activity_date, vessel_code, activity_time, activity_position))
  dataframe_detail <- subset(dataframe_detail, select = -c(vessel_code, min_distance, activity_time_bis, activity_date_time, vms_date_time))
  dataframe_detail <- dataframe_detail %>% dplyr::mutate(vms_crs = vms_crs, activity_crs = activity_crs)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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



# Shiny function : Error message if the trip selection elements are not correctly filled in
text_error_trip_select_server <- function(id, parent_in) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # if no selection element is filled in
      if (sum(isTruthy(parent_in$vessel_number), isTruthy(parent_in$trip_end_date)) == 0 && sum(isTruthy(parent_in$trip_start_date_range), isTruthy(parent_in$trip_end_date_range)) == 0) {
        return("Error: please select a trip")
      }
      # if there are elements filled in for several types of selection
      if (sum(isTruthy(parent_in$vessel_number), isTruthy(parent_in$trip_end_date)) > 0 && sum(isTruthy(parent_in$trip_start_date_range), isTruthy(parent_in$trip_end_date_range)) > 0) {
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
      return(TRUE)
    })
  })
}

# Shiny function : Read the .yml file of configuration for the connection
config_data_server <- function(id, parent_in) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
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

# Shiny function : Retrieves the list of trips selected by the user
trip_select_server <- function(id, parent_in, text_error_trip_select, config_data) {
  moduleServer(id, function(input, output, session) {
    eventReactive(input$start_button, {
      # If the connection data exists and there was no error in the trip selection, makes the connection
      req(config_data())
      if (text_error_trip_select() == TRUE) {
        data_connection <- furdeb::postgresql_dbconnection(
          db_user = config_data()[["databases_configuration"]][["observe_vmot6"]][["login"]],
          db_password = config_data()[["databases_configuration"]][["observe_vmot6"]][["password"]],
          db_dbname = config_data()[["databases_configuration"]][["observe_vmot6"]][["dbname"]],
          db_host = config_data()[["databases_configuration"]][["observe_vmot6"]][["host"]],
          db_port = config_data()[["databases_configuration"]][["observe_vmot6"]][["port"]]
        )
        # If the database is "observe", read, transform and execute the SQL query that selects the trips according to the user parameters
        if (any(grep("observe", data_connection[1]))) {
          # Selected trip with the vessel code and the end date of the trip
          if (isTruthy(parent_in$vessel_number) && isTruthy(parent_in$trip_end_date)) {
            trip_id_data <- furdeb::data_extraction(
              type = "database",
              file_path = system.file("sql",
                                      "trip_selected_vesselcode_enddate.sql",
                                      package = "AkadoR"
              ),
              database_connection = data_connection,
              anchor = list(select_item_1 = config_data()[["logbook_program"]], select_item_2 = as.character(parent_in$vessel_number), select_item_3 = parent_in$trip_end_date)
            )
          }
          # Selected trip with a date range
          if (isTruthy(parent_in$trip_start_date_range) && isTruthy(parent_in$trip_end_date_range)) {
            trip_id_data <- furdeb::data_extraction(
              type = "database",
              file_path = system.file("sql",
                                      "trip_selected_startdate_enddate.sql",
                                      package = "AkadoR"
              ),
              database_connection = data_connection,
              anchor = list(select_item_1 = config_data()[["logbook_program"]], select_item_2 = parent_in$trip_start_date_range, select_item_3 = parent_in$trip_end_date_range)
            )
          }
          # Disconnection to the base
          DBI::dbDisconnect(data_connection[[2]])
          # If trips have been found return them otherwise return FALSE
          if (dim(trip_id_data)[1] > 0) {
            return(trip_id_data)
          } else {
            return(FALSE)
          }
        } else {
          return(FALSE)
        }
      }
    })
  })
}

# Shiny function : Performs all calculations to test for inconsistencies
calcul_check_server <- function(id, text_error_trip_select, trip_select, config_data) {
  moduleServer(id, function(input, output, session) {
    # 0 - Global variables assignement ----
    trip_fishingtime <- NULL
    sum_route_fishingtime <- NULL
    trip_seatime <- NULL
    sum_route_seatime <- NULL
    vessel_capacity <- NULL
    trip_weighttotal <- NULL
    trip_landingtotalweight <- NULL
    sum_weightlanding <- NULL
    trip_id <- NULL
    trip_startdate <- NULL
    trip_enddate <- NULL
    button <- NULL
    harbour_label_departure <- NULL
    harbour_label_landing_trip_previous <- NULL
    harbour_id_landing_trip_previous <- NULL
    schooltype_code <- NULL
    association_object_count <- NULL
    vesselactivity_code <- NULL
    successstatus_code <- NULL
    activity_weight <- NULL
    type <- NULL
    ocean_label <- NULL
    sum_catch_weight <- NULL
    sum_measuredcount <- NULL
    sum_count <- NULL
    activity_seasurfacetemperature <- NULL
    sample_supersample <- NULL
    count_subsamplenumber_n0 <- NULL
    count_subsamplenumber_0 <- NULL
    count_subsamplenumber_1 <- NULL
    count_samplespecies <- NULL
    sample_well <- NULL
    sample_smallsweight <- NULL
    sample_bigsweight <- NULL
    sample_totalweight <- NULL
    little_percentage <- NULL
    big_percentage <- NULL
    measure1_percentage <- NULL
    measure2_percentage <- NULL
    sampletype_code <- NULL
    weightedweight <- NULL
    vesseltype_label <- NULL
    sum_landing_weight <- NULL
    weight_small_total <- NULL
    weight_big <- NULL
    activity_date <- NULL
    date_group <- NULL
    activity_id <- NULL
    nb_vms <- NULL
    min_distance <- NULL
    max_score <- NULL
    rf1 <- NULL
    activity_position <- NULL
    activity_position_display <- NULL
    grounding <- NULL
    activity_number <- NULL
    activity_position_prior <- NULL
    activity_position_post <- NULL
    # 1 - Data extraction ----
    reactive({
      # If there was no error in the trip selection and that there are trips for user settings, performs consistency tests
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select())) {
        # Connection to the base
        data_connection <- furdeb::postgresql_dbconnection(
          db_user = config_data()[["databases_configuration"]][["observe_vmot6"]][["login"]],
          db_password = config_data()[["databases_configuration"]][["observe_vmot6"]][["password"]],
          db_dbname = config_data()[["databases_configuration"]][["observe_vmot6"]][["dbname"]],
          db_host = config_data()[["databases_configuration"]][["observe_vmot6"]][["host"]],
          db_port = config_data()[["databases_configuration"]][["observe_vmot6"]][["port"]]
        )
        # If the database is "observe", read, transform and execute the SQL query that selects the trips according to the user parameters
        if (any(grep("observe", data_connection[1]))) {
          # Uses a function to extract data from activity
          activity_select <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "activity.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from sample
          sample_select <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "sample.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from sample species
          samplespecies_select <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "samplespecies.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from sample species measure
          samplespeciesmeasure_select <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "samplespeciesmeasure.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from well
          data_well <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "well.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from landing
          data_landing <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "landing.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from sample activity
          data_sampleactivity <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "sampleactivity.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = sample_select$sample_id)
          )
          # Uses a function to extract data from trip
          data_trip <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "trip.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item_1 = config_data()[["logbook_program"]], select_item_2 = trip_select()$trip_id)
          )
          # Uses a function to extract data from wellactivity
          wellactivity_select <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "wellactivity.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from wellactivityspecies
          data_wellactivityspecies <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "wellactivityspecies.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = wellactivity_select$wellactivity_id)
          )
          # Uses a function to extract data from route
          data_route <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "route.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = trip_select()$trip_id)
          )
          # Uses a function to extract data from tide
          data_tide <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "tide_id.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item_1 = config_data()[["logbook_program"]], select_item_2 = trip_select()$trip_id)
          )
          # Uses a function to extract data from catch of the tide
          data_catch_tide <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "catch_tide.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = data_tide$trip_id)
          )
          # Uses a function to extract data from landing of the tide
          data_landing_tide <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "landing_tide.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = data_tide$trip_id)
          )
          # Uses a function to extract data from activity_observedsystem
          data_activity_observedsystem <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "activity_observedsystem.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = activity_select$activity_id)
          )
          # Uses a function to extract data from activity_observedsystem
          data_catch <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "catch.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = activity_select$activity_id)
          )
          # Uses a function to extract data from activity_harbour
          data_activity_spatial <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "activity_spatial.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = activity_select$activity_id)
          )
          # Uses a function to extract data from transmittingbuoy
          data_transmittingbuoy <- furdeb::data_extraction(
            type = "database",
            file_path = system.file("sql",
                                    "transmittingbuoy.sql",
                                    package = "AkadoR"
            ),
            database_connection = data_connection,
            anchor = list(select_item = activity_select$activity_id)
          )
          # Disconnection to the bases
          DBI::dbDisconnect(data_connection[[2]])
          if (!is.null(config_data()[["databases_configuration"]][["vms"]])) {
            # Connection to the base VMS
            data_connection_vms <- furdeb::postgresql_dbconnection(
              db_user = config_data()[["databases_configuration"]][["vms"]][["login"]],
              db_password = config_data()[["databases_configuration"]][["vms"]][["password"]],
              db_dbname = config_data()[["databases_configuration"]][["vms"]][["dbname"]],
              db_host = config_data()[["databases_configuration"]][["vms"]][["host"]],
              db_port = config_data()[["databases_configuration"]][["vms"]][["port"]]
            )
            # Selection of unique activity dates and vessel numbers
            activity_select_vms <- unique(data.frame(vessel_code = activity_select$vessel_code, activity_date = activity_select$activity_date))
            # Uses a function to extract data from VMS
            data_vms <- furdeb::data_extraction(
              type = "database",
              file_path = system.file("sql",
                                      "vms.sql",
                                      package = "AkadoR"
              ),
              database_connection = data_connection_vms,
              anchor = list(select_item_1 = activity_select_vms$vessel_code, select_item_2 = activity_select_vms$activity_date)
            )
            # Force date type, otherwise empty dataframe sets to charactere format
            data_vms$vms_date <- as.Date(data_vms$vms_date)
            # Disconnection to the bases
            DBI::dbDisconnect(data_connection_vms[[2]])
          }
          # 2 - Data design ----
          # Create an intermediate dataset without information from previous trips to limit duplication problems in previous trips
          data_trip_unprecedented <- unique(subset(data_trip, select = -c(harbour_id_landing_trip_previous, harbour_label_landing_trip_previous)))
          # Retrieve trip : retrieve the vessel code, end of the trip, date of th activity and activity number of all the activity that have been selected
          colnames_trip_id <- c("trip_id", "vessel_code", "trip_enddate")
          # Retrieve trip activity : retrieve the vessel code, end of the trip, date of th activity and activity number of all the activity that have been selected
          colnames_activity_id <- c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_number", "vesselactivity_code")
          # Retrieve trip sample : retrieve the vessel code, end of the trip and sample number of all the sample that have been selected
          colnames_sample_id <- c("sample_id", "vessel_code", "trip_enddate", "sample_number")
          # Retrieve trip sample species : retrieve the vessel code, end of the trip, sample number, species FAO code and type of measure of all the sample that have been selected
          colnames_samplespecies_id <- c("samplespecies_id", "vessel_code", "trip_enddate", "sample_number", "species_fao_code", "sizemeasuretype_code")
          # Retrieve trip sample species measure : retrieve the vessel code, end of the trip, sample number, species FAO code and type of measure of all the sample that have been selected
          colnames_samplespeciesmeasure_id <- c("samplespeciesmeasure_id", "vessel_code", "trip_enddate", "sample_number", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass")
          # Checks data consistency
          if (nrow(data_trip) != length(trip_select()$trip_id)) {
            warning(text_object_more_or_less(id = trip_select()$trip_id, result_check = data_trip$trip_id))
          }
          # Uses a function which indicates whether the selected trips contain activities or not
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check trip activity inspector", sep = "")
          check_trip_activity_inspector_data <- check_trip_activity_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = activity_select, output = "report")
          # Uses a function to format the table
          check_trip_activity <- table_display_trip(check_trip_activity_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "warning")
          # Uses a function which indicates whether the selected trips contain fishing time inconsistent
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check fishing time inspector", sep = "")
          check_fishing_time_inspector_data <- check_fishing_time_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = data_route, output = "report")
          # Uses a function to format the table
          check_fishing_time <- table_display_trip(check_fishing_time_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_fishing_time <- dplyr::rename(
            .data = check_fishing_time,
            `Trip fishing time` = trip_fishingtime,
            `Sum route fishing time` = sum_route_fishingtime
          )
          # Uses a function which indicates whether the selected trips contain sea time inconsistent
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check sea time inspector", sep = "")
          check_sea_time_inspector_data <- check_sea_time_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = data_route, output = "report")
          # Uses a function to format the table
          check_sea_time <- table_display_trip(check_sea_time_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_sea_time <- dplyr::rename(
            .data = check_sea_time,
            `Trip sea time` = trip_seatime,
            `Sum route sea time` = sum_route_seatime
          )
          # Uses a function which indicates whether the selected trips contain landing total weight inconsistent with vessel capacity
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check landing consistent inspector", sep = "")
          check_landing_consistent_inspector_data <- check_landing_consistent_inspector(dataframe1 = data_trip_unprecedented, output = "report")
          # Uses a function to format the table
          check_landing_consistent <- table_display_trip(check_landing_consistent_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "warning")
          # Modify the table for display purposes: rename column
          check_landing_consistent <- dplyr::rename(
            .data = check_landing_consistent,
            `Vessel capacity` = vessel_capacity,
            `Total weight` = trip_weighttotal
          )
          # Uses a function which indicates whether the selected trips contain the total landed weight for canneries inconsistent with the weights of each landing for the canneries
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check landing total weight inspector", sep = "")
          check_landing_total_weight_inspector_data <- check_landing_total_weight_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = data_landing, output = "report", epsilon = config_data()[["epsilon"]])
          # Uses a function to format the table
          check_landing_total_weigh <- table_display_trip(check_landing_total_weight_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_landing_total_weigh <- dplyr::rename(
            .data = check_landing_total_weigh,
            `Trip landing weight` = trip_landingtotalweight,
            `Sum landing weight` = sum_weightlanding
          )
          # Uses a function which indicates whether the selected trips contain the trip start and end date inconsistent with the dates of activity
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check temporal limit inspector", sep = "")
          check_temporal_limit_inspector_data <- check_temporal_limit_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = data_route, output = "report")
          # Data preparation
          check_temporal_limit <- check_temporal_limit_inspector_data[[1]]
          check_temporal_limit_data_plot <- check_temporal_limit_inspector_data[[2]]
          # Add missing date
          check_temporal_limit_data_plot <- as.data.frame(check_temporal_limit_data_plot) %>%
            dplyr::group_by(trip_id) %>%
            tidyr::complete(activity_date = seq.Date(min(trip_startdate[1], trip_enddate[1]), max(trip_startdate[1], trip_enddate[1]), by = "day"), trip_startdate = trip_startdate[1], trip_enddate = trip_enddate[1])
          # Replaces NA for missing dates
          check_temporal_limit_data_plot <- check_temporal_limit_data_plot %>% tidyr::replace_na(list(inter_activity_date = TRUE, exter_activity_date = FALSE, count_freq = 0, logical = FALSE))
          check_temporal_limit_data_plot <- subset(check_temporal_limit_data_plot, select = -c(trip_enddate))
          # Add button and data for plot in table
          check_temporal_limit <- data_button_plot(data_plot = check_temporal_limit_data_plot, data_display = check_temporal_limit, data_id = trip_select(), colname_id = "trip_id", colname_plot = c("activity_date", "logical", "count_freq"), colname_info = c("trip_id", "vessel_code", "trip_startdate", "trip_enddate"), name_button = "button_temporal_limit")
          # Uses a function to format the table
          check_temporal_limit <- table_display_trip(check_temporal_limit, trip_select()[, colnames_trip_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_temporal_limit <- dplyr::rename(
            .data = check_temporal_limit,
            `Details problem` = button
          )
          # Uses a function which indicates whether the selected trips contain the trip harbour of departure of the current trip inconsistent with the harbour of landing of the previous trip
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check harbour inspector", sep = "")
          check_harbour_inspector_data <- check_harbour_inspector(dataframe1 = data_trip, output = "report")
          # Uses a function to format the table
          check_harbour <- table_display_trip(check_harbour_inspector_data, trip_select()[, colnames_trip_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_harbour <- dplyr::rename(
            .data = check_harbour,
            `Harbour landing` = harbour_label_landing_trip_previous,
            `Harbour departure` = harbour_label_departure
          )
          # Uses a function which indicates whether the selected trips contain RF1 inconsistent with threshold values
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check raising factor inspector", sep = "")
          check_raising_factor_inspector_data <- check_raising_factor_inspector(dataframe1 = data_trip_unprecedented, dataframe2 = data_catch_tide, dataframe3 = data_landing_tide, dataframe4 = data_tide, output = "report")
          # Uses a function to format the table
          check_raising_factor <- table_display_trip(check_raising_factor_inspector_data, trip_select()[,], type_inconsistency = "info")
          check_raising_factor$rf1 <- trunc(check_raising_factor$rf1 * 100000) / 100000
          # Modify the table for display purposes: rename column
          check_raising_factor <- dplyr::rename(
            .data = check_raising_factor,
            `Landing well status` = wellcontentstatus_landing_label,
            `RF1` = rf1
          )
          # Uses a function which indicates whether the school type is consistent with the association
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check fishing context inspector", sep = "")
          check_fishing_context_inspector_data <- check_fishing_context_inspector(dataframe1 = activity_select, dataframe2 = data_activity_observedsystem, output = "report")
          # Uses a function to format the table
          check_fishing_context <- table_display_trip(check_fishing_context_inspector_data, activity_select[, colnames_activity_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_fishing_context <- dplyr::rename(
            .data = check_fishing_context,
            `School type` = schooltype_code,
            `Number of associations object` = association_object_count
          )
          # Uses a function which indicates whether the succes status is consistent with the vessel activity, the type of school or the weight caught
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check operation inspector", sep = "")
          check_operationt_inspector_data <- check_operationt_inspector(dataframe1 = activity_select, output = "report")
          # Uses a function to format the table
          check_operationt <- table_display_trip(check_operationt_inspector_data, activity_select[, colnames_activity_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_operationt <- dplyr::rename(
            .data = check_operationt,
            `Vessel activity` = vesselactivity_code,
            `School type` = schooltype_code,
            `Success status` = successstatus_code,
            `Weigth` = activity_weight
          )
          # Uses a function which indicates whether the ocean declaration is consistent with activity position
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check position inspector", sep = "")
          check_position_inspector_data <- check_position_inspector(dataframe1 = activity_select, dataframe2 = data_trip, path_shp_sea = system.file("shp", "World_Seas", package = "AkadoR"), output = "report")
          # Add button and data for plot in table
          check_position <- data_button_plot(data_plot = check_position_inspector_data[[2]], data_display = check_position_inspector_data[[1]], data_id = activity_select[, colnames_activity_id], colname_id = "activity_id", colname_plot = c("activity_position", "activity_crs"), colname_info = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_number", "type", "ocean_label", "ocean_calculate"), name_button = "button_position")
          # Uses a function to format the table
          check_position <- table_display_trip(check_position, activity_select[, colnames_activity_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_position <- dplyr::rename(
            .data = check_position,
            `Type` = type,
            `Ocean trip` = ocean_label,
            `Ocean activity` = ocean_calculate,
            `Details problem` = button
          )
          # Uses a function which indicates whether that sum of the weight indicated for the catch is consistent with activity weight
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check weight inspector", sep = "")
          check_weight_inspector_data <- check_weight_inspector(dataframe1 = activity_select, dataframe2 = data_catch, output = "report")
          # Uses a function to format the table
          check_weight <- table_display_trip(check_weight_inspector_data, activity_select[, colnames_activity_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_weight <- dplyr::rename(
            .data = check_weight,
            `Activity weight` = activity_weight,
            `Sum catch weight` = sum_catch_weight
          )
          # Uses a function which indicates whether that size class of the samples depending on the species and measurement type is consistent with valid threshold
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check length class inspector", sep = "")
          check_length_class_inspector_data <- check_length_class_inspector(dataframe1 = samplespeciesmeasure_select, output = "report")
          # Uses a function to format the table
          check_length_class <- table_display_trip(check_length_class_inspector_data, samplespeciesmeasure_select[, colnames_samplespeciesmeasure_id], type_inconsistency = "error")
          # Uses a function which indicates whether that total number of individuals measured per sample is consistent with the sum of individuals per sample, species and size class
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check measure inspector", sep = "")
          check_measure_inspector_data <- check_measure_inspector(dataframe1 = samplespecies_select, dataframe2 = samplespeciesmeasure_select, output = "report")
          # Uses a function to format the table
          check_measure <- table_display_trip(check_measure_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_measure <- dplyr::rename(
            .data = check_measure,
            `Number individuals measured sample` = sum_measuredcount,
            `Sum numbers individuals size class` = sum_count
          )
          # Uses a function which indicates whether that sea surface temperature is consistent with the valid threshold
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check temperature inspector", sep = "")
          check_temperature_inspector_data <- check_temperature_inspector(dataframe1 = activity_select, output = "report")
          # Uses a function to format the table
          check_temperature <- table_display_trip(check_temperature_inspector_data, activity_select[, colnames_activity_id], type_inconsistency = "error")
          check_temperature <- dplyr::rename(
            .data = check_temperature,
            `Sea surface temperature` = activity_seasurfacetemperature
          )
          # Uses a function which indicates whether that catch weight for activity is consistent with the sample weighting
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check weighting sample inspector", sep = "")
          check_weighting_sample_inspector_data <- check_weighting_sample_inspector(dataframe1 = data_catch, dataframe2 = data_sampleactivity, output = "report")
          # Uses a function to format the table
          check_weighting_sample <- table_display_trip(check_weighting_sample_inspector_data, activity_select[, colnames_activity_id], type_inconsistency = "error")
          check_weighting_sample <- dplyr::rename(
            .data = check_weighting_sample,
            `Sum catch weight ` = weight,
            `Sum sample weighted weight`  = weightedweight
          )
          # Uses a function which indicates whether that species sampled is consistent with species authorized
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check species inspector", sep = "")
          check_species_inspector_data <- check_species_inspector(dataframe1 = samplespecies_select, output = "report")
          # Uses a function to format the table
          check_species <- table_display_trip(check_species_inspector_data, samplespecies_select[, colnames_samplespecies_id], type_inconsistency = "error")
          # Uses a function which indicates whether the sample is consistent with the presence of measurement
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check sample without measure inspector", sep = "")
          check_sample_without_measure_inspector_data <- check_sample_without_measure_inspector(dataframe1 = samplespecies_select, dataframe2 = samplespeciesmeasure_select, output = "report")
          # Uses a function to format the table
          check_sample_without_measure <- table_display_trip(check_sample_without_measure_inspector_data, samplespecies_select[, colnames_samplespecies_id], type_inconsistency = "error")
          # Uses a function which indicates whether the sample is consistent with the presence of species
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check sample without species inspector", sep = "")
          check_sample_without_species_inspector_data <- check_sample_without_species_inspector(dataframe1 = sample_select, dataframe2 = samplespecies_select, output = "report")
          # Uses a function to format the table
          check_sample_without_species <- table_display_trip(check_sample_without_species_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          # Uses a function which indicates whether the sample is consistent with the subsample number
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check super sample number consistent inspector", sep = "")
          check_super_sample_number_consistent_inspector_data <- check_super_sample_number_consistent_inspector(dataframe1 = sample_select, dataframe2 = samplespecies_select, output = "report")
          # Uses a function to format the table
          check_super_sample_number_consistent <- table_display_trip(check_super_sample_number_consistent_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_super_sample_number_consistent <- dplyr::rename(
            .data = check_super_sample_number_consistent,
            `Super sample` = sample_supersample,
            `Counts number sub-sample numbers not 0` = count_subsamplenumber_n0,
            `Counts number sub-sample numbers equal 0` = count_subsamplenumber_0,
            `Counts number sub-sample numbers equal 1` = count_subsamplenumber_1,
            `Counts number sample species` = count_samplespecies
          )
          # Uses a function which indicates whether the sample well number is consistent with the associated trip well numbers
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check well number consistent inspector", sep = "")
          check_well_number_consistent_inspector_data <- check_well_number_consistent_inspector(dataframe1 = sample_select, dataframe2 = data_well, output = "report")
          # Uses a function to format the table
          check_well_number_consistent <- table_display_trip(check_well_number_consistent_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_well_number_consistent <- dplyr::rename(
            .data = check_well_number_consistent,
            `Well` = sample_well
          )
          # Uses a function which indicates whether the sample is consistent for the percentage of little and big fish sampled
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check little big inspector", sep = "")
          check_little_big_inspector_data <- check_little_big_inspector(dataframe1 = sample_select, dataframe2 = samplespecies_select, dataframe3 = samplespeciesmeasure_select, output = "report")
          # Uses a function to format the table
          check_little_big <- table_display_trip(check_little_big_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_little_big$little_percentage <- trunc(check_little_big$little_percentage * 1000) / 1000
          check_little_big$big_percentage <- trunc(check_little_big$big_percentage * 1000) / 1000
          check_little_big$measure1_percentage <- trunc(check_little_big$measure1_percentage * 1000) / 1000
          check_little_big$measure2_percentage <- trunc(check_little_big$measure2_percentage * 1000) / 1000
          check_little_big <- dplyr::rename(
            .data = check_little_big,
            `Small fish weight` = sample_smallsweight,
            `Big fish weight` = sample_bigsweight,
            `Total weight` = sample_totalweight,
            `Little %` = little_percentage,
            `Big %` = big_percentage,
            `Measurement type FL %` = measure1_percentage,
            `Measurement type PD1 %` = measure2_percentage
          )
          # Uses a function which indicates whether the sample is consistent for the weighting
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check weighting inspector", sep = "")
          check_weighting_inspector_data <- check_weighting_inspector(dataframe1 = sample_select, dataframe2 = data_sampleactivity, dataframe3 = data_trip_unprecedented, dataframe4 = data_landing, output = "report")
          # Uses a function to format the table
          check_weighting <- table_display_trip(check_weighting_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_weighting <- dplyr::rename(
            .data = check_weighting,
            `Small fish weight` = sample_smallsweight,
            `Big fish weight` = sample_bigsweight,
            `Total weight` = sample_totalweight,
            `Sample type` = sampletype_code,
            `Sum weighted weights` = weightedweight,
            `Vessel type` = vesseltype_label,
            `Sum weight fresh landings baitboats` = sum_landing_weight
          )
          # Uses a function which indicates whether the sample weight (m10 and p10) is consistent for the global weight
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check weight sample inspector", sep = "")
          check_weight_sample_inspector_data <- check_weight_sample_inspector(dataframe1 = sample_select, output = "report")
          # Uses a function to format the table
          check_weight_sample <- table_display_trip(check_weight_sample_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_weight_sample <- dplyr::rename(
            .data = check_weight_sample,
            `Small fish weight` = sample_smallsweight,
            `Big fish weight` = sample_bigsweight,
            `Total weight` = sample_totalweight
          )
          # Uses a function which indicates whether the sample and the existence of the activity
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check activity sample inspector", sep = "")
          check_activity_sample_inspector_data <- check_activity_sample_inspector(dataframe1 = sample_select, dataframe2 = data_sampleactivity, output = "report")
          # Uses a function to format the table
          check_activity_sample <- table_display_trip(check_activity_sample_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          # Uses a function which indicates whether the sample measurement types is consistent for the species or weight values
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check ldlf inspector", sep = "")
          check_ldlf_inspector_data <- check_ldlf_inspector(dataframe1 = samplespecies_select, dataframe2 = sample_select, output = "report")
          # Uses a function to format the table
          check_ldlf <- table_display_trip(check_ldlf_inspector_data, samplespecies_select[, colnames_samplespecies_id], type_inconsistency = "error")
          check_ldlf <- dplyr::rename(
            .data = check_ldlf,
            `Small fish weight` = sample_smallsweight,
            `Big fish weight` = sample_bigsweight,
            `Total weight` = sample_totalweight
          )
          # Uses a function which indicates whether the small and large sample weights is consistent for the sum of the small and big weights of the associated well
          message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check distribution inspector", sep = "")
          check_distribution_inspector_data <- check_distribution_inspector(dataframe1 = sample_select, dataframe2 = data_well, dataframe3 = wellactivity_select, dataframe4 = data_wellactivityspecies, output = "report")
          # Uses a function to format the table
          check_distribution <- table_display_trip(check_distribution_inspector_data, sample_select[, colnames_sample_id], type_inconsistency = "error")
          check_distribution <- dplyr::rename(
            .data = check_distribution,
            `Small fish weight` = sample_smallsweight,
            `Big fish weight` = sample_bigsweight,
            `Well` = sample_well,
            `Small fish weight well` = weight_small_total,
            `Big fish weight well` = weight_big
          )
          # Uses a function which indicates whether the activity position is consistent for VMS position
          if (exists("data_vms")) {
            message(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Start check anapo inspector", sep = "")
            # Recovers all trip positions
            check_anapo_inspector_data <- check_anapo_inspector(dataframe1 = activity_select, dataframe2 = data_activity_spatial, dataframe3 = data_vms, activity_crs = ifelse(length(stats::na.omit(unique(activity_select$activity_crs))) == 0, 4326, stats::na.omit(unique(activity_select$activity_crs))), vms_crs = ifelse(length(stats::na.omit(unique(data_vms$vms_crs))) == 0, 4326, stats::na.omit(unique(data_vms$vms_crs))), output = "report")
            check_anapo_inspector_dataplot <- merge(check_anapo_inspector_data[[2]], activity_select[, c("vessel_code", "trip_enddate", "activity_id", "trip_id", "activity_number", "vesselactivity_code")], by = "activity_id")
            # Add information on whether the activity is linked to a grounding (object or buoy) or not in data plot
            data_tmp_grounding <- column_grounding(data = check_anapo_inspector_dataplot, data_transmittingbuoy = data_transmittingbuoy)
            check_anapo_inspector_dataplot <- merge(check_anapo_inspector_dataplot, data_tmp_grounding, by = "activity_id")
            # Selecting useful data for the plot
            check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot %>%
              dplyr::select("trip_id", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "vesselactivity_code", "activity_crs", "grounding") %>%
              dplyr::group_by(trip_id) %>%
              dplyr::distinct()
            # Add position information for activities n, n-1 and n+1 (not just related to grounding)
            check_anapo_inspector_data_table <- check_anapo_inspector_data[[1]]
            rm(check_anapo_inspector_data)
            check_anapo_inspector_data_table <- merge(check_anapo_inspector_data_table, check_anapo_inspector_dataplot_trip[, c("trip_id", "activity_id", "activity_date", "activity_number", "grounding", "activity_position")], by = "activity_id")
            check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>% dplyr::mutate(activity_position_display = gsub("POINT\\(|\\)", "", activity_position))
            check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
              dplyr::mutate(activity_position_prior = replace(activity_position_display, grounding, NA)) %>%
              dplyr::group_by(trip_id) %>%
              dplyr::arrange(activity_date, activity_number) %>%
              tidyr::fill(activity_position_prior, .direction = "down") %>%
              dplyr::mutate(activity_position_prior = dplyr::lag(activity_position_prior))
            check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
              dplyr::mutate(activity_position_post = replace(activity_position_display, grounding, NA)) %>%
              dplyr::group_by(trip_id) %>%
              dplyr::arrange(activity_date, activity_number) %>%
              tidyr::fill(activity_position_post, .direction = "up") %>%
              dplyr::mutate(activity_position_post = dplyr::lead(activity_position_post))
            check_anapo_inspector_data_table <- check_anapo_inspector_data_table %>%
              dplyr::ungroup() %>%
              dplyr::select(-c("trip_id", "activity_date", "activity_number", "activity_position"))
            # Retrieves activity positions for the previous, current and next day
            check_anapo_inspector_dataplot_trip <- check_anapo_inspector_dataplot_trip %>%
              dplyr::select(-c("activity_id")) %>%
              dplyr::mutate(date_group = activity_date)
            check_anapo_inspector_dataplot_trip_prior <- check_anapo_inspector_dataplot_trip %>% dplyr::mutate(date_group = activity_date - 1)
            check_anapo_inspector_dataplot_trip_post <- check_anapo_inspector_dataplot_trip %>% dplyr::mutate(date_group = activity_date + 1)
            check_anapo_inspector_dataplot_range_date <- rbind(check_anapo_inspector_dataplot_trip, check_anapo_inspector_dataplot_trip_prior, check_anapo_inspector_dataplot_trip_post) %>%
              dplyr::group_by(date_group, trip_id) %>%
              dplyr::distinct()
            check_anapo_inspector_dataplot_range_date <- merge(check_anapo_inspector_dataplot_range_date, activity_select[, c("activity_date", "trip_id", "activity_id")], by.x = c("date_group", "trip_id"), by.y = c("activity_date", "trip_id"))
            check_anapo_inspector_dataplot_range_date <- check_anapo_inspector_dataplot_range_date %>%
              dplyr::group_by(date_group, trip_id, activity_id) %>%
              dplyr::distinct()
            code_txt <- data_to_text(name_data = "check_anapo_inspector_dataplot_range_date", name_col = "trip_data", name_button = "NULL", colname_id = "activity_id", colname_plot = c("activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code"), colname_info = NULL)
            eval(parse(text = code_txt))
            # Data formatting controlled activity
            check_anapo_inspector_dataplot_activity <- check_anapo_inspector_dataplot %>%
              dplyr::select(c("vessel_code", "trip_enddate", "activity_id", "activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code")) %>%
              dplyr::distinct()
            code_txt <- data_to_text(name_data = "check_anapo_inspector_dataplot_activity", name_col = "activity_data", name_button = "NULL", colname_id = "activity_id", colname_plot = c("vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_position", "activity_number", "grounding", "vesselactivity_code"), colname_info = NULL)
            eval(parse(text = code_txt))
            check_anapo_inspector_dataplot <- check_anapo_inspector_dataplot %>% dplyr::select(-c("vessel_code", "trip_enddate", "activity_number", "activity_time", "vesselactivity_code"))
            check_anapo_inspector_dataplot <- merge(check_anapo_inspector_dataplot, check_anapo_inspector_dataplot_range_date, by = "activity_id")
            check_anapo_inspector_dataplot <- merge(check_anapo_inspector_dataplot, check_anapo_inspector_dataplot_activity, by = "activity_id")
            check_anapo_inspector_dataplot <- check_anapo_inspector_dataplot %>% tibble::as_tibble()
            code_txt <- data_to_text(name_data = "check_anapo_inspector_dataplot", name_col = "data_plot", name_button = "NULL", colname_id = "activity_id", colname_plot = c("vms_position", "vms_date", "vms_time", "distance", "duration", "score"), colname_info = c("activity_id", "activity_crs", "vms_crs", "activity_data", "trip_data"))
            eval(parse(text = code_txt))
            # Number of the table containing the Anapo plot information in calcul_check_server
            check_anapo_inspector_dataplot$num_table <- 29
            check_anapo_inspector_dataplot$num_row <- seq_len(nrow(check_anapo_inspector_dataplot))
            # Add button and data for plot in table
            check_anapo <- data_button_plot(data_plot = check_anapo_inspector_dataplot, data_display = check_anapo_inspector_data_table, data_id = activity_select[, colnames_activity_id], colname_id = "activity_id", colname_plot = NULL, colname_info = c("num_table", "num_row"), name_button = "button_anapo", choice_select_row = "all")
            # Uses a function to format the table
            check_anapo <- table_display_trip(check_anapo, activity_select[, colnames_activity_id], type_inconsistency = "error")
            check_anapo$min_distance <- trunc(check_anapo$min_distance * 1000) / 1000
            check_anapo$max_score <- trunc(check_anapo$max_score * 1000) / 1000
            check_anapo <- dplyr::rename(
              .data = check_anapo,
              `Number of VMS` = nb_vms,
              `Minimale distance` = min_distance,
              `Maximum score` = max_score,
              `Grounding` = grounding,
              `Position activity` = activity_position_display,
              `Position previous activity (not grounding)` = activity_position_prior,
              `Position next activity (not grounding)` = activity_position_post,
              `Details problem` = button
            )
          } else {
            check_anapo <- data.frame()
            check_anapo_inspector_dataplot <- data.frame()
          }
          return(list(check_trip_activity, check_fishing_time, check_sea_time, check_landing_consistent, check_landing_total_weigh, check_temporal_limit, check_harbour, check_raising_factor, check_fishing_context, check_operationt, check_position, check_weight, check_length_class, check_measure, check_temperature, check_weighting_sample, check_species, check_sample_without_measure, check_sample_without_species, check_super_sample_number_consistent, check_well_number_consistent, check_little_big, check_weighting, check_weight_sample, check_activity_sample, check_ldlf, check_distribution, check_anapo, check_anapo_inspector_dataplot))
        }
      }
    })
  })
}

# Shiny function : Displays the errors and notifications that occur when you want to start the calculation
error_trip_select_serveur <- function(id, text_error_trip_select, config_data, trip_select, calcul_check) {
  moduleServer(id, function(input, output, session) {
    output$text <- renderText({
      # If there are errors in the selection parameters
      if (!text_error_trip_select()) {
        showNotification(id = "notif_warning", ui = text_error_trip_select(), type = "error")
        return(paste0("<span style=\"color:red\">", text_error_trip_select(), "</span>"))
      }
      # If the connection file is missing
      if (text_error_trip_select() && !isTruthy(config_data())) {
        text <- "Error: There is no configuration file for the connection to the base"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0("<span style=\"color:red\">", text, ", please either select one using the \"settings\" tab or put it in ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"), "</span>"))
      }
      # If the selected trip is not found in the database
      if (text_error_trip_select() == TRUE && !is.data.frame(trip_select()) && trip_select() == FALSE) {
        text <- "Error: no trip was found for these parameters"
        showNotification(id = "notif_warning", ui = text, type = "error")
        return(paste0("<span style=\"color:red\">", text, "</span>"))
      }
      # If the different manipulations on the data are finished
      if (isTruthy(calcul_check())) {
        text <- "Calculation finished"
        showNotification(id = "notif_default", ui = text, type = "default")
        cat(format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Process AkadoR ran successfully.\n", sep = "")
        return(paste0("<span style=\"color:#34C909\">", text, "</span>"))
      }
    })
  })
}

# Shiny function : table display
table_server <- function(id, data, number, parent_in, text_error_trip_select, trip_select, calcul_check, column_no_wrap = NULL) {
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- data()[[number]]
        if (parent_in$type_line_check_trip == "inconsistent") {
          data <- data[data$Check != as.character(icon("check")), ]
        }
        data <- DT::datatable(data,
                              escape = FALSE,
                              rownames = FALSE,
                              extensions = "Buttons",
                              options = list(
                                lengthChange = FALSE, scrollX = TRUE, dom = "Bfrtip",
                                buttons =
                                  list(
                                    list(extend = "copy", text = "Copy data displayed"),
                                    list(extend = "collection", text = "Download all data", action = DT::JS(paste0("function ( e, dt, node, config ) {Shiny.setInputValue('button_download', ", number, ", {priority: 'event'});}")))
                                  )
                              )
        ) %>% DT::formatStyle(columns = column_no_wrap, "white-space" = "nowrap")
        return(data)
      }
    })
  })
}

window_button_download <- function(number) {
  modalDialog(downloadButton(outputId = "download_csv", label = "CSV"),
              downloadButton(outputId = "download_excel", label = "Excel"),
              fade = TRUE,
              easyClose = TRUE,
              footer = NULL,
              title = "Download Table"
  )
}

table_ui <- function(id, title, size_box = "col-sm-12 col-md-6 col-lg-6", text = NULL) {
  div(
    id = paste0("div_", id),
    class = size_box,
    shinydashboard::box(
      width = "100%",
      title = title,
      status = "primary",
      solidHeader = TRUE,
      HTML(text),
      shinycssloaders::withSpinner(DT::DTOutput(NS(namespace = id, id = "table")), type = 6, size = 0.5, proxy.height = "70px")
    )
  )
}

# Function which formats the trip data for display inconsistency
table_display_trip <- function(data, data_info, type_inconsistency) {
  # Global variables assignement
  vessel_code <- NULL
  trip_enddate <- NULL
  activity_date <- NULL
  activity_time <- NULL
  activity_number <- NULL
  vesselactivity_code <- NULL
  sample_number <- NULL
  species_fao_code <- NULL
  sizemeasuretype_code <- NULL
  samplespeciesmeasure_sizeclass <- NULL
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Deletes duplicate columns
  all_colname <- c(colnames(data), colnames(data_info))
  colname_double <- table(all_colname)[table(all_colname) > 1]
  colname_double <- names(colname_double)[!(names(colname_double) %in% colname_id)]
  data <- data[, !(colnames(data) %in% colname_double)]
  # Combines the consistency test on the data and data identification information
  data <- merge(data_info, data, by = colname_id)
  # Sort rows by date
  data <- data[order(data$trip_enddate), ]
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
  # Modify the table for display purposes: rename column
  data <- dplyr::rename(
    .data = data,
    `Vessel code` = vessel_code,
    `Trip enddate` = trip_enddate,
    Check = logical
  )
  # Modify the table for display purposes specifically for activities : rename column
  if (length(grep("^activity_", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `Activity date` = activity_date,
      `Activity time` = activity_time,
      `Activity number` = activity_number,
      `Vessel activity` = vesselactivity_code
    )
  }
  # Modify the table for display purposes specifically for samples : rename column
  if (length(grep("^sample", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `Sample number` = sample_number
    )
  }
  # Modify the table for display purposes specifically for samples species : rename column
  if (length(grep("^samplespecies", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `FAO code` = species_fao_code,
      `Size measure type` = sizemeasuretype_code
    )
  }
  # Modify the table for display purposes specifically for samples species measure : rename column
  if (length(grep("^samplespeciesmeasure", colnames(data), value = TRUE)) != 0) {
    data <- dplyr::rename(
      .data = data,
      `Size class` = samplespeciesmeasure_sizeclass
    )
  }
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Modify the table for display purposes: delete column
  data <- subset(data, select = -c(eval(parse(text = colname_id))))
  return(data)
}

# Function to create a data.frame in character
data_to_text <- function(name_data, name_col, name_button, colname_id, colname_plot, colname_info) {
  code_txt <- paste0(name_data, " <-", name_data, "%>%dplyr::group_by(", colname_id, ") %>%
            dplyr::reframe(", name_col, " = paste0(", name_button, ",paste0(deparse(dplyr::across(.cols=c(", paste0(colname_plot, collapse = ","), '))), collapse = "")', ifelse(is.null(colname_info), "", paste0(',"&",', paste0("unique(", colname_info ,")", collapse = ', "&",'))), "))")
  return(code_txt)
}

# Function to create the button in the table that will create the plot
data_button_plot <- function(data_plot, data_display, data_id, colname_id, colname_plot, colname_info, name_button, choice_select_row = "error") {
  # Global variables assignement
  buttontmp <- NULL
  # Arguments verification
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
      output = "message"
    ))
  }
  # Add line identification
  data_plot <- merge(data_id, data_plot, by.x = colname_id, by.y = colname_id, all.x = TRUE)
  # Add button and data for plot in table
  code_txt <- data_to_text(name_data = "data_plot", name_col = "buttontmp", name_button = '"button&"', colname_id = colname_id, colname_plot = colname_plot, colname_info = colname_info)
  eval(parse(text = code_txt))
  data_display <- merge(data_display, data_plot, by = colname_id)
  # Select the lines that will display a plot
  if (choice_select_row == "all") {
    select_row <- rep(TRUE, nrow(data_display))
  }
  if (choice_select_row == "error") {
    select_row <- data_display$logical == FALSE
  }
  if (choice_select_row == "valid") {
    select_row <- data_display$logical == TRUE
  }
  data_display$button <- NA
  data_display$button[select_row] <- sapply(which(select_row), function(c) {
    as.character(shiny::actionButton(inputId = data_display$buttontmp[c], label = "Detail", onclick = paste0('Shiny.setInputValue(\"', name_button, '\", this.id, {priority: \"event\"})')))
  })
  data_display <- subset(data_display, select = -c(buttontmp))
  return(data_display)
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

# Function to create the plot of the consistency of the position for the activity
plot_position <- function(data) {
  # Local binding global variables
  . <- NULL
  # Spatial formatting
  if(!is.na(data$activity_position)){
    data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = unique(data$activity_crs)) %>% dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
    # Plot
    plotly::plot_ly(
      data = data_geo, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", marker = list(size = 10), hovertemplate = "(%{lat}\u00B0,%{lon}\u00B0)<extra></extra>"
    ) %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron", center = list(lon = data_geo$X, lat = data_geo$Y)))
  }else{
    # Plot
    plotly::plot_ly(type = "scattermapbox", mode = "markers", marker = list(size = 10), hovertemplate = "(%{lat}\u00B0,%{lon}\u00B0)<extra></extra>") %>%
      plotly::layout(showlegend = FALSE, mapbox = list(style = "carto-positron"))
  }
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
  # Remove missing position in vms
  data_vms <- data_vms %>% dplyr::filter(!is.na(data_vms$vms_position))
  # Format date time and order
  if (!all(is.na(data_vms$vms_position))) {
    data_vms <- data_vms %>% dplyr::mutate(date_time = as.POSIXct(paste(vms_date, vms_time)))
    data_vms <- data_vms[order(data_vms$date_time), ]
  }
  data_trip <- data_trip %>% dplyr::mutate(date_time = as.POSIXct(paste(activity_date, activity_time)))
  data_trip <- data_trip[order(data_trip$date_time), ]
  # Spatial formatting
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_vms[!is.na(data_vms$vms_position), ] %>%
      sf::st_as_sf(wkt = "vms_position", crs = as.numeric(crs_vms)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
  }
  if (!all(is.na(data_activity$activity_position))) {
    data_geo_activity <- data_activity[!is.na(data_activity$activity_position), ] %>%
      sf::st_as_sf(wkt = "activity_position", crs = as.numeric(crs_activity)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
  }
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip <- data_trip[!is.na(data_trip$activity_position), ] %>%
      sf::st_as_sf(wkt = "activity_position", crs = as.numeric(crs_activity)) %>%
      dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
  }
  # text hovertemplate
  if (!all(is.na(data_vms$vms_position))) {
    data_geo_vms <- data_geo_vms %>% dplyr::mutate(text = paste("Date:", vms_date, "<br>Time:", vms_time, "<br>Distance:", trunc(distance * 1000) / 1000, "miles<br>Duration:", trunc((duration / 60000) * 1000) / 1000, "minutes<br>Score:", trunc(score * 1000) / 1000))
  }
  if (!all(is.na(data_activity$activity_position))) {
    data_geo_activity <- data_geo_activity %>% dplyr::mutate(text = paste("Date:", activity_date, "<br>Time:", activity_time, "<br>Activity number:", activity_number, "<br>Vessel activity:", vesselactivity_code, "<br>Position:%{lat}\u00B0,%{lon}\u00B0", "<br>Grounding:", grounding, "<extra></extra>"))
  }
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip <- data_geo_trip %>% dplyr::mutate(text = paste("Date:", activity_date, "<br>Time:", activity_time, "<br>Activity number:", activity_number, "<br>Vessel activity:", vesselactivity_code, "<br>Grounding:", grounding))
  }
  # Plot
  plot <- plotly::plot_ly() %>%
    plotly::layout(mapbox = list(style = "carto-positron", pitch = 0, zoom = 6))
  if (!all(is.na(data_trip$activity_position))) {
    data_geo_trip_grounding <- data_geo_trip %>% dplyr::filter(grounding)
    data_geo_trip_nongrounding <- data_geo_trip %>% dplyr::filter(!grounding)
    plot <- plot %>%
      plotly::add_trace(name = "Activity (solely grounding object)", data = data_geo_trip_grounding, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", text = ~text, hovertemplate = "%{text}<br>Position:%{lat}\u00B0,%{lon}\u00B0<extra></extra>", marker = list(color = "rgb(142, 52, 0)", size = 10))
    plot <- plot %>%
      plotly::add_trace(name = "Activity", data = data_geo_trip_nongrounding, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", text = ~text, hovertemplate = "%{text}<br>Position:%{lat}\u00B0,%{lon}\u00B0<extra></extra>", marker = list(color = grDevices::colorRampPalette(c("#B2FF00", "#006415"))(nrow(data_geo_trip_nongrounding)), size = 10), line = list(color = "#11BC00"))
  }
  if (!all(is.na(data_vms$vms_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "VMS day", data = data_geo_vms, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "lines+markers", text = ~text, hovertemplate = "%{text}<br>Position:%{lat}\u00B0,%{lon}\u00B0<extra></extra>", marker = list(color = grDevices::colorRampPalette(c("#00F7FF", "#3B18AA"))(nrow(data_geo_vms)), size = 10), line = list(color = "#0032FF"))%>%
      plotly::layout(mapbox = list(center = list(lon = data_geo_vms$X[1], lat = data_geo_vms$Y[1])))
  }
  if (!all(is.na(data_activity$activity_position))) {
    plot <- plot %>%
      plotly::add_trace(name = "Suspicious activity", data = data_geo_activity, lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers", hovertemplate = ~text, marker = list(color = "rgb(255, 0, 0)", size = 10)) %>%
      plotly::layout(mapbox = list(center = list(lon = data_geo_activity$X, lat = data_geo_activity$Y)))
  }
  return(plot)
}

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
      output = "message"
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
      output = "message"
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
      output = "message"
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
  if ((sum(data$grounding, na.rm = TRUE) + sum(!data$grounding, na.rm = TRUE)) != nrow_first || sum(is.na(data$grounding)) > 0) {
    all <- c(select, data$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(data$grounding)) > 0) {
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
  if (sum(number_occurrences == 1) > 0) {
    text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
  }
  if (sum(number_occurrences > 2) > 0) {
    text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
  }
  return(text)
}
