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
  ) != TRUE & class(data_connection) != "data.frame") {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Class for \"data_connection\" must be a *list* in the case of a connection to a base and a *data.frame* otherwise.\n ",
      sep = ""
    )
  } else {
    if (r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "logical"
    ) == TRUE && !is.data.frame(data_connection[[1]]) && r_type_checking(
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
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
      readLines(file.path(".", "sql", "trip_with_activity.sql")),
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
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_with_activity_data <- data_connection[[1]]
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
  }
  # 3 - Data design ----
  nrow_first <- length(trip_id)
  # Search trip ID in the associations trip, route, activity
  comparison <- vector_comparison(
    first_vector = trip_id,
    second_vector = trip_with_activity_data$trip_id,
    comparison_type = "difference",
    output = "report"
  )
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
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Class for \"data_connection\" must be a *list* with either the connection information or the two data frames.\n ",
        sep = ""
      )
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
      readLines(file.path(".", "sql", "trip_fishingtime.sql")),
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
      readLines(file.path(".", "sql", "route_fishingtime.sql")),
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
  comparison <- vector_comparison(
    first_vector = trip_fishingtime_data$trip_idfishing_time,
    second_vector = route_fishingtime_data$trip_idfishing_time,
    comparison_type = "difference",
    output = "report"
  )
  # Modify the table for display purposes: add, remove and order column
  trip_fishingtime_data <- merge(trip_fishingtime_data, comparison, by.x = "trip_idfishing_time", by.y = "first_vector")
  trip_fishingtime_data <- dplyr::relocate(.data = trip_fishingtime_data, trip_fishing_time, .after = logical)
  route_fishingtime_data <- subset(route_fishingtime_data, select = -c(trip_idfishing_time))
  trip_fishingtime_data <- subset(trip_fishingtime_data, select = -c(trip_idfishing_time))
  trip_fishingtime_data <- merge(trip_fishingtime_data, route_fishingtime_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Management of missing fishing time values
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
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Class for \"data_connection\" must be a *list* with either the connection information or the two data frames.\n ",
        sep = ""
      )
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
      readLines(file.path(".", "sql", "trip_seatime.sql")),
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
      readLines(file.path(".", "sql", "route_seatime.sql")),
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
  comparison <- vector_comparison(
    first_vector = trip_seatime_data$trip_idsea_time,
    second_vector = route_seatime_data$trip_idsea_time,
    comparison_type = "difference",
    output = "report"
  )
  # Modify the table for display purposes: add, remove and order column
  trip_seatime_data <- merge(trip_seatime_data, comparison, by.x = "trip_idsea_time", by.y = "first_vector")
  trip_seatime_data <- dplyr::relocate(.data = trip_seatime_data, trip_sea_time, .after = logical)
  route_seatime_data <- subset(route_seatime_data, select = -c(trip_idsea_time))
  trip_seatime_data <- subset(trip_seatime_data, select = -c(trip_idsea_time))
  trip_seatime_data <- merge(trip_seatime_data, route_seatime_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Management of missing sea time values
  trip_seatime_data[is.na(trip_seatime_data$trip_sea_time), "logical"] <- FALSE
  # Management of the 0 value for the time at sea
  trip_seatime_data[!is.na(trip_seatime_data$trip_sea_time) & trip_seatime_data$trip_sea_time == 0, "logical"] <- FALSE
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


# Function that tests if the vessel capacity is consistent with the landing total weight, in the future integrated in the pakage codama
check_landing_consistent_inspector <- function(data_connection,
                                               type_select,
                                               select,
                                               output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE & class(data_connection) != "data.frame") {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Class for \"data_connection\" must be a *list* in the case of a connection to a base and a *data.frame* otherwise.\n ",
      sep = ""
    )
  } else {
    if (r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "logical"
    ) == TRUE && !is.data.frame(data_connection[[1]]) && r_type_checking(
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
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
    # landing total weight and vessel capacity link to trip
    trip_weight_capacity_sql <- paste(
      readLines(file.path("sql", "trip_weight_vessel_capacity.sql")),
      collapse = "\n"
    )
    trip_weight_capacity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_weight_capacity_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_weight_capacity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_weight_capacity_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_weight_capacity_sql <- data_connection[[1]]
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
  nrow_first <- nrow(trip_weight_capacity_data)
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  trip_weight_capacity_data$trip_weighttotal <- rowSums(x = trip_weight_capacity_data[, c("trip_landingtotalweight", "trip_localmarkettotalweight")], na.rm = TRUE)
  # Converts cubic meters to tons
  trip_weight_capacity_data$vessel_capacity <- trip_weight_capacity_data$vessel_capacity * 0.7
  # Compare landing total weight of the trip with vessel capacity
  comparison <- vector_comparison(
    first_vector = trip_weight_capacity_data$trip_weighttotal,
    second_vector = trip_weight_capacity_data$vessel_capacity,
    comparison_type = "less",
    output = "report"
  )
  trip_weight_capacity_data$logical <- comparison$logical
  trip_weight_capacity_data <- subset(trip_weight_capacity_data, select = -c(trip_landingtotalweight, trip_localmarkettotalweight))
  # Management of missing vessel capacity
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$vessel_capacity), "logical"] <- FALSE
  # Management of the 0 value for vessel capacity
  trip_weight_capacity_data[!is.na(trip_weight_capacity_data$vessel_capacity) & trip_weight_capacity_data$vessel_capacity == 0, "logical"] <- FALSE
  if ((sum(trip_weight_capacity_data$logical) + sum(!trip_weight_capacity_data$logical)) != nrow_first) {
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
    return(print(paste0("There are ", sum(!trip_weight_capacity_data$logical), " trips with vessel capacity smaller than the landing total weight")))
  }
  if (output == "report") {
    return(trip_weight_capacity_data)
  }
  if (output == "logical") {
    if (sum(!trip_weight_capacity_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if the total landed weight for canneries is consistent with the weights of each landing for the canneries, in the future integrated in the pakage codama
check_landing_total_weight_inspector <- function(data_connection,
                                                 type_select,
                                                 select,
                                                 output,
                                                 epsilon = 0.01) {
  # 0 - Global variables assignement ----
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
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Class for \"data_connection\" must be a *list* with either the connection information or the two data frames.\n ",
        sep = ""
      )
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
    # landing total weight and vessel capacity link to trip
    trip_weight_capacity_sql <- paste(
      readLines(file.path("sql", "trip_weight_vessel_capacity.sql")),
      collapse = "\n"
    )
    trip_weight_capacity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_weight_capacity_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_weight_capacity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_weight_capacity_sql
    ))
    trip_weight_landing_sql <- paste(
      readLines(file.path("sql", "trip_weight_landing.sql")),
      collapse = "\n"
    )
    trip_weight_landing_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_weight_landing_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_weight_landing_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_weight_landing_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_weight_capacity_data <- data_connection[[1]]
      trip_weight_landing_data <- data_connection[[2]]
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
  nrow_first <- nrow(trip_weight_capacity_data)
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  trip_weight_landing_data <- trip_weight_landing_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_weightlanding = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge and calcul difference
  trip_weight_capacity_data <- merge(trip_weight_capacity_data, trip_weight_landing_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  trip_weight_capacity_data$difference <- ifelse(is.na(trip_weight_capacity_data$trip_landingtotalweight), 0, trip_weight_capacity_data$trip_landingtotalweight) - ifelse(is.na(trip_weight_capacity_data$sum_weightlanding), 0, trip_weight_capacity_data$sum_weightlanding)
  trip_weight_capacity_data$difference <- abs(trip_weight_capacity_data$difference)
  trip_weight_capacity_data$epsilon <- epsilon
  # Compare trip IDs and weight landing of the trip or the sum of the landing
  comparison <- vector_comparison(
    first_vector = trip_weight_capacity_data$difference,
    second_vector = trip_weight_capacity_data$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  trip_weight_capacity_data$logical <- comparison$logical
  trip_weight_capacity_data <- subset(trip_weight_capacity_data, select = -c(vessel_capacity, trip_localmarkettotalweight, difference, epsilon))
  # Management of missing landing weight in trip
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$trip_landingtotalweight), "logical"] <- FALSE
  # Management of missing sum of the landing
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$sum_weightlanding) & !is.na(trip_weight_capacity_data$trip_landingtotalweight) & trip_weight_capacity_data$trip_landingtotalweight > 0, "logical"] <- FALSE
  if ((sum(trip_weight_capacity_data$logical) + sum(!trip_weight_capacity_data$logical)) != nrow_first) {
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
    return(print(paste0("There are ", sum(!trip_weight_capacity_data$logical), " trips with a landing weight for the canneries different from the sum of the weights of each landing for the canneries")))
  }
  if (output == "report") {
    return(trip_weight_capacity_data)
  }
  if (output == "logical") {
    if (sum(!trip_weight_capacity_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if trip start and end date is consistent with the dates of activity, in the future integrated in the pakage codama
check_temporal_limit_inspector <- function(data_connection,
                                           type_select,
                                           select,
                                           output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE & class(data_connection) != "data.frame") {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Class for \"data_connection\" must be a *list* in the case of a connection to a base and a *data.frame* otherwise.\n ",
      sep = ""
    )
  } else {
    if (r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "logical"
    ) == TRUE && !is.data.frame(data_connection[[1]]) && r_type_checking(
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
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
        readLines(con = file.path(
          "sql",
          "trip_id_selected_by_year.sql"
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
    # trip start and end date and date of route
    trip_date_activity_sql <- paste(
      readLines(file.path("sql", "trip_date_activity.sql")),
      collapse = "\n"
    )
    trip_date_activity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_date_activity_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_date_activity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_date_activity_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_date_activity_data <- data_connection[[1]]
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
  nrow_first <- length(unique(select_sql))
  # Calculate the temporal indicator per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  trip_date_activity_data_detail <- trip_date_activity_data
  # Calculation if the date is in the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_greater_equal <- vector_comparison(
    first_vector = trip_date_activity_data$activity_date,
    second_vector = trip_date_activity_data$trip_startdate,
    comparison_type = "greater_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_less_equal <- vector_comparison(
    first_vector = trip_date_activity_data$activity_date,
    second_vector = trip_date_activity_data$trip_enddate,
    comparison_type = "less_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$inter_activity_date <- trip_date_activity_data_detail$trip_startdate_greater_equal & trip_date_activity_data_detail$trip_enddate_less_equal
  # Calculation if the date is outside the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_less <- vector_comparison(
    first_vector = trip_date_activity_data$activity_date,
    second_vector = trip_date_activity_data$trip_startdate,
    comparison_type = "less",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_greater <- vector_comparison(
    first_vector = trip_date_activity_data$activity_date,
    second_vector = trip_date_activity_data$trip_enddate,
    comparison_type = "greater",
    output = "report"
  )$logical
  trip_date_activity_data_detail$exter_activity_date <- trip_date_activity_data_detail$trip_startdate_less | trip_date_activity_data_detail$trip_enddate_greater
  # Calculates the number of occurrences of each activity date
  trip_date_activity_data_detail <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate, activity_date, inter_activity_date, exter_activity_date, ) %>%
    dplyr::summarise(count_freq = length(activity_date), .groups = "keep")
  # Calculation if an inconsistency among the different tests on the date has been found
  trip_date_activity_data_detail$logical <- trip_date_activity_data_detail$inter_activity_date & !trip_date_activity_data_detail$exter_activity_date & trip_date_activity_data_detail$count_freq == 1
  # Calculation if the number of days is consistent and if there are inconsistencies in the dates for the trips
  trip_date_activity_data <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(nb_day = length(activity_date), logical_tmp = sum(!logical) == 0, .groups = "keep")
  # Calculation if an inconsistency among the different tests on the trip has been found
  trip_date_activity_data <- trip_date_activity_data %>%
    dplyr::summarise(logical = sum(c(!((1 + trip_enddate - trip_startdate) == nb_day), !logical_tmp)) == 0, .groups = "keep")
  # Management of missing trip start and end date
  trip_date_activity_data[is.na(trip_date_activity_data$trip_startdate) | is.na(trip_date_activity_data$trip_enddate), "logical"] <- FALSE
  trip_date_activity_data <- subset(trip_date_activity_data, select = -c(trip_startdate, trip_enddate))
  if ((sum(trip_date_activity_data$logical) + sum(!trip_date_activity_data$logical)) != nrow_first) {
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
    return(print(paste0("There are ", sum(!trip_date_activity_data$logical), " trips with a landing weight for the canneries different from the sum of the weights of each landing for the canneries")))
  }
  if (output == "report") {
    return(list(trip_date_activity_data, trip_date_activity_data_detail))
  }
  if (output == "logical") {
    if (sum(!trip_date_activity_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if the harbour of landing of the previous trip and the harbour of departure of the current trip is the same, in the future integrated in the pakage codama
check_harbour_inspector <- function(data_connection,
                                    type_select,
                                    select,
                                    output,
                                    logbook_program) {
  # 0 - Global variables assignement ----
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
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Class for \"data_connection\" must be a *list* with either the connection information or the three data frames.\n ",
        sep = ""
      )
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  # Checks the type of logbook_program
  if (r_type_checking(
    r_object = logbook_program,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = logbook_program,
      type = "character",
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
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
    # Retrieves the identifier of the previous trip
    trip_previous_trip_sql <- paste(
      readLines(file.path("sql", "trip_previous.sql")),
      collapse = "\n"
    )
    trip_previous_trip_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_previous_trip_sql,
      select_item_1 = DBI::SQL(paste(paste0("'", logbook_program, "'"), collapse = ", ")),
      select_item_2 = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    trip_previous_trip_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_previous_trip_sql
    ))
    # Retrieves the port of landing of the previous trip
    select_sql <- paste0("'", trip_previous_trip_data$trip_previous_id, "'")
    landing_harbour_sql <- paste(
      readLines(file.path("sql", "trip_landing_harbour.sql")),
      collapse = "\n"
    )
    landing_harbour_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = landing_harbour_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    landing_harbour_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = landing_harbour_sql
    ))
    landing_harbour_data <- dplyr::rename(
      .data = landing_harbour_data,
      harbour_id_landing = harbour_id,
    )
    # Retrieves the port of departure of the trip
    select_sql <- paste0("'", trip_previous_trip_data$trip_id, "'")
    departure_harbour_sql <- paste(
      readLines(file.path("sql", "trip_departure_harbour.sql")),
      collapse = "\n"
    )
    departure_harbour_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = departure_harbour_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    departure_harbour_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = departure_harbour_sql
    ))
    departure_harbour_data <- dplyr::rename(
      .data = departure_harbour_data,
      harbour_id_departure = harbour_id,
    )
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_previous_trip_data <- data_connection[[1]]
      landing_harbour_data <- data_connection[[2]]
      departure_harbour_data <- data_connection[[3]]
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
  nrow_first <- nrow(trip_previous_trip_data)
  # merge data
  trip_previous_trip_data <- merge(trip_previous_trip_data, landing_harbour_data, by.x = "trip_previous_id", by.y = "trip_id", all.x = TRUE)
  trip_previous_trip_data <- merge(trip_previous_trip_data, departure_harbour_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE, suffixes = c("_landing", "_departure"))
  # Compare landing total weight of the trip with vessel capacity
  comparison <- vector_comparison(
    first_vector = trip_previous_trip_data$harbour_id_departure,
    second_vector = trip_previous_trip_data$harbour_id_landing,
    comparison_type = "equal",
    output = "report"
  )
  trip_previous_trip_data$logical <- comparison$logical
  trip_previous_trip_data <- subset(trip_previous_trip_data, select = -c(trip_previous_id, harbour_id_landing, harbour_id_departure))
  # Management of missing vessel capacity
  trip_previous_trip_data[is.na(trip_previous_trip_data$harbour_id_departure), "logical"] <- FALSE
  trip_previous_trip_data[is.na(trip_previous_trip_data$harbour_id_landing), "logical"] <- FALSE
  if ((sum(trip_previous_trip_data$logical) + sum(!trip_previous_trip_data$logical)) != nrow_first) {
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
    return(print(paste0("There are ", sum(!trip_previous_trip_data$logical), " trips with departure port different from the landing harbour of a previous trip")))
  }
  if (output == "report") {
    return(trip_previous_trip_data)
  }
  if (output == "logical") {
    if (sum(!trip_previous_trip_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


# Function which formats the trip data for display inconsistency
table_display_trip <- function(data, data_trip, type_inconsistency) {
  # Combines the consistency test on the data and data trip identification information
  data <- merge(data_trip, data, by.x = "trip_id", by.y = "trip_id")
  # Modify the table for display purposes: delete column
  data <- subset(data, select = -c(trip_id))
  # Add icons according to the success of the test
  data$logical[data$logical == TRUE] <- as.character(icon("check"))
  if (type_inconsistency == "error") {
    data$logical[data$logical == FALSE] <- as.character(icon("xmark"))
  }
  if (type_inconsistency == "warning") {
    data$logical[data$logical == FALSE] <- as.character(icon("exclamation"))
  }
  return(data)
}


# Function to create the plot of the consistency of the dates by trip
plot_temporal_limit <- function(data) {
  # Deletes the rows where the date of the activity is missing
  data<-data[!is.na(data$activity_date),]
  plotly::plot_ly() %>%
    plotly::add_markers(x = c(data$trip_startdate[1], data$trip_enddate[1]), y = c(1, 1), marker = list(
      color = "#63A9FF", symbol = "circle"
    ), name = "start date and end date", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    plotly::add_markers(data = subset(data, logical == TRUE), x = ~activity_date, y = ~count_freq, marker = list(
      color = "#18ED84", symbol = "cross-thin-open"
    ), name = "date activity good", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    plotly::add_markers(data = subset(data, logical == FALSE), x = ~activity_date, y = ~count_freq, marker = list(
      color = "#FF7320", symbol = "x-thin-open"
    ), name = "date activity bad", hovertemplate = paste("%{x|%b %d, %Y}<extra></extra>")) %>%
    layout(
      xaxis = list(
        title = "Date",
        dtick = 86400000.0 * 5,
        tickformat = "%b %d"
      ),
      yaxis = list(
        title = "Occurence",
        tickvals = c(data$count_freq, 1),
        ticktext = c(data$count_freq, 1)
      )
    )
}
