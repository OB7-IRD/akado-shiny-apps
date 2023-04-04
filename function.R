# Function that tests the presence of activity associated with the trip, in the future integrated in the pakage codama
check_trip_activity_inspector <- function(data_connection,
                                          type_select,
                                          select,
                                          output) {
  # 0 - Global variables assignement ----
  first_vector <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "message"
    ))
  } else {
    if (r_type_checking(
      r_object = data_connection[[2]],
      type = "PostgreSQLConnection",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = data_connection[[2]],
        type = "PostgreSQLConnection",
        output = "message"
      ))
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_values = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_values = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_values = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_values = c("trip", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "trip" &&
        r_type_checking(
          r_object = select,
          type = "character",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "character",
        output = "message"
      ))
    }
    if (type_select == "year" &&
        r_type_checking(
          r_object = select,
          type = "numeric",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "numeric",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Trip selection in the SQL query
    if (type_select == "trip") {
      select_sql <- paste0("'", select, "'")
      trip_id <- select
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Trip with a departure or arrival date in the selected year
      trip_id_selected_by_year_sql <- paste(
        readLines(con = system.file("sql",
                                    "trip_id_selected_by_year.sql",
                                    package = "codama"
        )),
        collapse = "\n"
      )
      trip_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = trip_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      trip_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = trip_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", trip_id_selected_by_year_data$trip_id, "'")
      trip_id <- trip_id_selected_by_year_data$trip_id
    }
    # Trip associated with route and activity
    trip_with_activity_sql <- paste(
      readLines("./sql/trip_with_activity.sql"),
      collapse = "\n"
    )
    trip_with_activity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_with_activity_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_with_activity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_with_activity_sql
    ))
  } else {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Consistency check not developed yet for this \"data_connection\" argument.\n ",
      sep = ""
    )
  }
  # 3 - Data design ----
  nrow_first <- length(trip_id)
  # Search trip ID in the associations trip, route, activity
  comparison <- vectors_comparisons(trip_id,
                                    trip_with_activity_data$trip_id,
                                    comparison_type = "difference",
                                    output = "report"
  )
  comparison$logical <- FALSE
  colnames_comparison <- grep("vectors_comparisons_", colnames(comparison))
  comparison$logical[comparison[, colnames_comparison] == "no difference"] <- TRUE
  trip_id <- cbind(trip_id, comparison)
  if ((sum(trip_id$logical) + sum(!trip_id$logical)) != nrow_first) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      sep = ""
    )
  }
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!trip_id$logical), " trip with no activity")))
  }
  if (output == "report") {
    trip_id <- subset(trip_id, select = -c(first_vector))
    return(trip_id)
  }
  if (output == "logical") {
    if (sum(!trip_id$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests the fishing time, in the future integrated in the pakage codama
check_fishing_time_inspector <- function(data_connection,
                                         type_select,
                                         select,
                                         output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  route_fishing_time <- NULL
  trip_fishingtime_data <- NULL
  trip_fishing_time <- NULL
  trip_idfishing_time <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "message"
    ))
  } else {
    if (!is.data.frame(data_connection[[1]]) && r_type_checking(
      r_object = data_connection[[2]],
      type = "PostgreSQLConnection",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = data_connection[[2]],
        type = "PostgreSQLConnection",
        output = "message"
      ))
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_values = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_values = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_values = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_values = c("trip", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "trip" &&
        r_type_checking(
          r_object = select,
          type = "character",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "character",
        output = "message"
      ))
    }
    if (type_select == "year" &&
        r_type_checking(
          r_object = select,
          type = "numeric",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "numeric",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Trip selection in the SQL query
    if (type_select == "trip") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Trip with a departure or arrival date in the selected year
      trip_id_selected_by_year_sql <- paste(
        readLines(con = system.file("sql",
                                    "trip_id_selected_by_year.sql",
                                    package = "codama"
        )),
        collapse = "\n"
      )
      trip_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = trip_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      trip_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = trip_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", trip_id_selected_by_year_data$trip_id, "'")
    }
    # Fishing time link to trip
    trip_fishingtime_sql <- paste(
      readLines(con = system.file("sql",
                                  "trip_fishingtime.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    trip_fishingtime_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_fishingtime_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_fishingtime_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_fishingtime_sql
    ))
    # Fishing time link to route
    route_fishingtime_sql <- paste(
      readLines(con = system.file("sql",
                                  "route_fishingtime.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    route_fishingtime_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = route_fishingtime_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    route_fishingtime_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = route_fishingtime_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_fishingtime_data <- data_connection[[1]]
      route_fishingtime_data <- data_connection[[2]]
    } else {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Consistency check not developed yet for this \"data_connection\" argument, you can provide both sets of data instead.\n ",
        sep = ""
      )
    }
  }
  # 3 - Data design ----
  nrow_first <- nrow(trip_fishingtime_data)
  # Calculate the sum of the fishing time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  route_fishingtime_data <- route_fishingtime_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_fishingtime = ifelse(all(is.na(route_fishing_time)), route_fishing_time[NA_integer_], sum(route_fishing_time, na.rm = TRUE)))
  # Group the pair to compare
  route_fishingtime_data$trip_idfishing_time <- paste0(route_fishingtime_data$trip_id, route_fishingtime_data$sum_route_fishingtime)
  trip_fishingtime_data$trip_idfishing_time <- paste0(trip_fishingtime_data$trip_id, trip_fishingtime_data$trip_fishing_time)
  # Compare trip IDs and fishing time of the trip or the sum of the route
  comparison <- vectors_comparisons(trip_fishingtime_data$trip_idfishing_time,
                                    route_fishingtime_data$trip_idfishing_time,
                                    comparison_type = "difference",
                                    output = "report"
  )
  comparison$logical <- FALSE
  # Modify the table for display purposes: add, remove and order column
  colnames_comparison <- grep("vectors_comparisons_", colnames(comparison))
  comparison$logical[comparison[, colnames_comparison] == "no difference"] <- TRUE
  trip_fishingtime_data <- merge(trip_fishingtime_data, comparison, by.x = "trip_idfishing_time", by.y = "first_vector")
  trip_fishingtime_data <- dplyr::relocate(.data = trip_fishingtime_data, trip_fishing_time, .after = logical)
  route_fishingtime_data <- subset(route_fishingtime_data, select = -c(trip_idfishing_time))
  trip_fishingtime_data <- subset(trip_fishingtime_data, select = -c(trip_idfishing_time))
  trip_fishingtime_data <- merge(trip_fishingtime_data, route_fishingtime_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Management of missing fishing time values
  colnames_comparison <- grep("vectors_comparisons_", colnames(trip_fishingtime_data))
  trip_fishingtime_data[is.na(trip_fishingtime_data$trip_fishing_time), colnames_comparison] <- "trip fishing time is missing"
  trip_fishingtime_data[is.na(trip_fishingtime_data$trip_fishing_time), "logical"] <- FALSE
  if ((sum(trip_fishingtime_data$logical) + sum(!trip_fishingtime_data$logical)) != nrow_first) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!trip_fishingtime_data$logical), " trip with a fishing time different from the sum of the fishing times of each activity")))
  }
  if (output == "report") {
    return(trip_fishingtime_data)
  }
  if (output == "logical") {
    if (sum(!trip_fishingtime_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests the sea time, in the future integrated in the pakage codama
check_sea_time_inspector <- function(data_connection,
                                     type_select,
                                     select,
                                     output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  route_sea_time <- NULL
  trip_seatime_data <- NULL
  trip_sea_time <- NULL
  trip_idsea_time <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "message"
    ))
  } else {
    if (!is.data.frame(data_connection[[1]]) && r_type_checking(
      r_object = data_connection[[2]],
      type = "PostgreSQLConnection",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = data_connection[[2]],
        type = "PostgreSQLConnection",
        output = "message"
      ))
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_values = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_values = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_values = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_values = c("trip", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "trip" &&
        r_type_checking(
          r_object = select,
          type = "character",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "character",
        output = "message"
      ))
    }
    if (type_select == "year" &&
        r_type_checking(
          r_object = select,
          type = "numeric",
          output = "logical"
        ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "numeric",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Trip selection in the SQL query
    if (type_select == "trip") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Trip with a departure or arrival date in the selected year
      trip_id_selected_by_year_sql <- paste(
        readLines(con = system.file("sql",
                                    "trip_id_selected_by_year.sql",
                                    package = "codama"
        )),
        collapse = "\n"
      )
      trip_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = trip_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      trip_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = trip_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", trip_id_selected_by_year_data$trip_id, "'")
    }
    # sea time link to trip
    trip_seatime_sql <- paste(
      readLines("./sql/trip_seatime.sql"),
      collapse = "\n"
    )
    trip_seatime_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_seatime_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_seatime_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_seatime_sql
    ))
    # sea time link to route
    route_seatime_sql <- paste(
      readLines("./sql/route_seatime.sql"),
      collapse = "\n"
    )
    route_seatime_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = route_seatime_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    route_seatime_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = route_seatime_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_seatime_data <- data_connection[[1]]
      route_seatime_data <- data_connection[[2]]
    } else {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Consistency check not developed yet for this \"data_connection\" argument, you can provide both sets of data instead.\n ",
        sep = ""
      )
    }
  }
  # 3 - Data design ----
  nrow_first <- nrow(trip_seatime_data)
  # Calculate the sum of the sea time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  route_seatime_data <- route_seatime_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_seatime = ifelse(all(is.na(route_sea_time)), route_sea_time[NA_integer_], sum(route_sea_time, na.rm = TRUE)))
  # Group the pair to compare
  route_seatime_data$trip_idsea_time <- paste0(route_seatime_data$trip_id, route_seatime_data$sum_route_seatime)
  trip_seatime_data$trip_idsea_time <- paste0(trip_seatime_data$trip_id, trip_seatime_data$trip_sea_time)
  # Compare trip IDs and sea time of the trip or the sum of the route
  comparison <- vectors_comparisons(trip_seatime_data$trip_idsea_time,
                                    route_seatime_data$trip_idsea_time,
                                    comparison_type = "difference",
                                    output = "report"
  )
  comparison$logical <- FALSE
  # Modify the table for display purposes: add, remove and order column
  colnames_comparison <- grep("vectors_comparisons_", colnames(comparison))
  comparison$logical[comparison[, colnames_comparison] == "no difference"] <- TRUE
  trip_seatime_data <- merge(trip_seatime_data, comparison, by.x = "trip_idsea_time", by.y = "first_vector")
  trip_seatime_data <- dplyr::relocate(.data = trip_seatime_data, trip_sea_time, .after = logical)
  route_seatime_data <- subset(route_seatime_data, select = -c(trip_idsea_time))
  trip_seatime_data <- subset(trip_seatime_data, select = -c(trip_idsea_time))
  trip_seatime_data <- merge(trip_seatime_data, route_seatime_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Management of missing sea time values
  colnames_comparison <- grep("vectors_comparisons_", colnames(trip_seatime_data))
  trip_seatime_data[is.na(trip_seatime_data$trip_sea_time), colnames_comparison] <- "trip sea time is missing"
  trip_seatime_data[is.na(trip_seatime_data$trip_sea_time), "logical"] <- FALSE
  # Management of the 0 value for the time at sea
  colnames_comparison <- grep("vectors_comparisons_", colnames(trip_seatime_data))
  trip_seatime_data[trip_seatime_data$trip_sea_time == 0, colnames_comparison] <- "trip sea time is 0"
  trip_seatime_data[trip_seatime_data$trip_sea_time == 0, "logical"] <- FALSE
  if ((sum(trip_seatime_data$logical) + sum(!trip_seatime_data$logical)) != nrow_first) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!trip_seatime_data$logical), " trip with a sea time different from the sum of the sea times of each activity")))
  }
  if (output == "report") {
    return(trip_seatime_data)
  }
  if (output == "logical") {
    if (sum(!trip_seatime_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function which formats the trip data for display
table_display_trip<-function(data,data_trip){
  # Combines the consistency test on the data and data trip identification information
  data <- merge(data_trip, data, by.x = "trip_id", by.y = "trip_id")
  # Modify the table for display purposes: delete column
  colnames_comparison <- grep("vectors_comparisons_", colnames(data))
  data <- data[, -c(colnames_comparison)]
  data <- subset(data, select = -c(trip_id))
  # Add icons according to the success of the test
  data$logical[data$logical == TRUE] <- as.character(icon("check"))
  data$logical[data$logical == FALSE] <- as.character(icon("xmark"))
  return(data)
}
