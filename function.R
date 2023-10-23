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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(select_sql)
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_with_activity_data <- data_connection[[1]]
      nrow_first <- nrow(trip_with_activity_data)
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
  # Search trip ID in the associations trip, route, activity
  comparison <- vector_comparison(
    first_vector = trip_id,
    second_vector = trip_with_activity_data$trip_id,
    comparison_type = "difference",
    output = "report"
  )
  trip_id <- cbind(trip_id, comparison)
  if ((sum(trip_id$logical) + sum(!trip_id$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_id$trip_id)},
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
  route_fishingtime <- NULL
  trip_fishingtime_data <- NULL
  trip_fishingtime <- NULL
  trip_idfishingtime <- NULL
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_fishingtime_data <- data_connection[[1]]
      route_fishingtime_data <- data_connection[[2]]
      nrow_first <- nrow(trip_fishingtime_data)
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
  # Calculate the sum of the fishing time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  route_fishingtime_data <- route_fishingtime_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_fishingtime = ifelse(all(is.na(route_fishingtime)), route_fishingtime[NA_integer_], sum(route_fishingtime, na.rm = TRUE)))
  # Group the pair to compare
  trip_fishingtime_data <- merge(trip_fishingtime_data, route_fishingtime_data, by = "trip_id", all.x = TRUE)
  # Compare fishing time of the trip or the sum of the route
  comparison <- vector_comparison(
    trip_fishingtime_data$trip_fishingtime,
    trip_fishingtime_data$sum_route_fishingtime,
    comparison_type = "equal",
    output = "report"
  )
  # Modify the table for display purposes: add, remove and order column
  trip_fishingtime_data <- cbind(trip_fishingtime_data, comparison)
  trip_fishingtime_data <- dplyr::relocate(.data = trip_fishingtime_data, trip_fishingtime, sum_route_fishingtime, .after = logical)
  trip_fishingtime_data <- subset(trip_fishingtime_data, select = -c(first_vector, second_vector))
  if ((sum(trip_fishingtime_data$logical) + sum(!trip_fishingtime_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_fishingtime_data$trip_id)},
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
  route_seatime <- NULL
  trip_seatime_data <- NULL
  trip_seatime <- NULL
  trip_idseatime <- NULL
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_seatime_data <- data_connection[[1]]
      route_seatime_data <- data_connection[[2]]
      nrow_first <- nrow(trip_seatime_data)
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
  # Calculate the sum of the sea time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  route_seatime_data <- route_seatime_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_seatime = ifelse(all(is.na(route_seatime)), route_seatime[NA_integer_], sum(route_seatime, na.rm = TRUE)))
  # Group the pair to compare
  trip_seatime_data <- merge(trip_seatime_data, route_seatime_data, by = "trip_id", all.x = TRUE)
  # Compare trip IDs and sea time of the trip or the sum of the route
  comparison <- vector_comparison(
    first_vector = trip_seatime_data$trip_seatime,
    second_vector = trip_seatime_data$sum_route_seatime,
    comparison_type = "equal",
    output = "report"
  )
  # Modify the table for display purposes: add, remove and order column
  trip_seatime_data <- cbind(trip_seatime_data, comparison)
  trip_seatime_data <- dplyr::relocate(.data = trip_seatime_data, trip_seatime, sum_route_seatime, .after = logical)
  trip_seatime_data <- subset(trip_seatime_data, select = -c(first_vector, second_vector))
  # Management of the 0 value for the time at sea
  trip_seatime_data[!is.na(trip_seatime_data$trip_seatime) & trip_seatime_data$trip_seatime == 0, "logical"] <- FALSE
  if ((sum(trip_seatime_data$logical) + sum(!trip_seatime_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_seatime_data$trip_id)},
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_weight_capacity_data <- data_connection[[1]]
      nrow_first <- nrow(trip_weight_capacity_data)
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
  trip_weight_capacity_data <- dplyr::relocate(.data = trip_weight_capacity_data, vessel_capacity, trip_weighttotal, .after = logical)
  trip_weight_capacity_data <- subset(trip_weight_capacity_data, select = -c(trip_landingtotalweight, trip_localmarkettotalweight))
  # Management of missing vessel capacity
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$vessel_capacity), "logical"] <- FALSE
  # Management of the 0 value for vessel capacity
  trip_weight_capacity_data[!is.na(trip_weight_capacity_data$vessel_capacity) & trip_weight_capacity_data$vessel_capacity == 0, "logical"] <- FALSE
  if ((sum(trip_weight_capacity_data$logical) + sum(!trip_weight_capacity_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_weight_capacity_data$trip_id)},
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      trip_weight_capacity_data <- data_connection[[1]]
      trip_weight_landing_data <- data_connection[[2]]
      nrow_first <- nrow(trip_weight_capacity_data)
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
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  trip_weight_landing_data <- trip_weight_landing_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_weightlanding = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge and calcul difference
  trip_weight_capacity_data <- merge(trip_weight_capacity_data, trip_weight_landing_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  trip_weight_capacity_data$difference <- ifelse(is.na(trip_weight_capacity_data$trip_landingtotalweight), 0, trip_weight_capacity_data$trip_landingtotalweight) - ifelse(is.na(trip_weight_capacity_data$sum_weightlanding), 0, trip_weight_capacity_data$sum_weightlanding)
  trip_weight_capacity_data$difference <- abs(trip_weight_capacity_data$difference)
  trip_weight_capacity_data$epsilon <- epsilon
  # Compare sum difference with epsilon
  comparison <- vector_comparison(
    first_vector = trip_weight_capacity_data$difference,
    second_vector = trip_weight_capacity_data$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  trip_weight_capacity_data$logical <- comparison$logical
  trip_weight_capacity_data <- dplyr::relocate(.data = trip_weight_capacity_data, trip_landingtotalweight, sum_weightlanding, .after = logical)
  trip_weight_capacity_data <- subset(trip_weight_capacity_data, select = -c(vessel_capacity, trip_localmarkettotalweight, difference, epsilon))
  # Management of missing landing weight in trip
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$trip_landingtotalweight), "logical"] <- FALSE
  # Management of missing sum of the landing
  trip_weight_capacity_data[is.na(trip_weight_capacity_data$sum_weightlanding) & !is.na(trip_weight_capacity_data$trip_landingtotalweight) & trip_weight_capacity_data$trip_landingtotalweight > 0, "logical"] <- FALSE
  if ((sum(trip_weight_capacity_data$logical) + sum(!trip_weight_capacity_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_weight_capacity_data$trip_id)},
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      trip_date_activity_data <- data_connection[[1]]
      nrow_first <- length(unique(trip_date_activity_data$trip_id))
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
    dplyr::group_by(trip_id, trip_startdate, trip_enddate, activity_date, inter_activity_date, exter_activity_date) %>%
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
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_date_activity_data$trip_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!trip_date_activity_data$logical), " trips with missing or surplus days")))
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
  if (any(grep("observe_", data_connection[1]))) {
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
    nrow_first <- length(unique(select))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE && is.data.frame(data_connection[[3]]) == TRUE) {
      trip_previous_trip_data <- data_connection[[1]]
      landing_harbour_data <- data_connection[[2]]
      departure_harbour_data <- data_connection[[3]]
      nrow_first <- nrow(trip_previous_trip_data)
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
  # Merge data
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
  trip_previous_trip_data <- dplyr::relocate(.data = trip_previous_trip_data, harbour_name_landing, harbour_name_departure, .after = logical)
  trip_previous_trip_data <- subset(trip_previous_trip_data, select = -c(trip_previous_id, harbour_id_landing, harbour_id_departure))
  # Management of missing vessel capacity
  trip_previous_trip_data[is.na(trip_previous_trip_data$harbour_id_departure), "logical"] <- FALSE
  trip_previous_trip_data[is.na(trip_previous_trip_data$harbour_id_landing), "logical"] <- FALSE
  if ((sum(trip_previous_trip_data$logical) + sum(!trip_previous_trip_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,trip_previous_trip_data$trip_id)},
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

# Function that tests if RF1 is within the limit values, in the future integrated in the pakage codama
check_raising_factor_inspector <- function(data_connection,
                                           type_select,
                                           select,
                                           output,
                                           logbook_program,
                                           species,
                                           limit = c(0.9, 1.1)) {
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
  # Checks the type of limit
  if (r_type_checking(
    r_object = limit,
    type = "numeric",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = limit,
      type = "numeric",
      length = 2L,
      output = "message"
    ))
  }
  if (any(grep("observe_", data_connection[1]))) {
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
    # Checks the type of species
    if (r_type_checking(
      r_object = species,
      type = "character",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = species,
        type = "character",
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
    # Retrieves all identifiers for the entire tide
    tide_id_sql <- paste(
      readLines(file.path("sql", "tide_id.sql")),
      collapse = "\n"
    )
    tide_id_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = tide_id_sql,
      select_item_1 = DBI::SQL(paste(paste0("'", logbook_program, "'"), collapse = ", ")),
      select_item_2 = DBI::SQL(paste(select_sql, collapse = ", "))
    )
    tide_id_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = tide_id_sql
    ))
    # Retrieves the weight of each catch in the tide
    select_trip_tide_sql <- paste0("'", tide_id_data$trip_id, "'")
    catch_weight_RF1_sql <- paste(
      readLines(file.path("sql", "trip_catch_weight_RF1.sql")),
      collapse = "\n"
    )
    catch_weight_RF1_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = catch_weight_RF1_sql,
      select_item_1 = DBI::SQL(paste(select_trip_tide_sql, collapse = ", ")),
      select_item_2 = DBI::SQL(paste(paste0("'", species, "'"), collapse = ", "))
    )
    catch_weight_RF1_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = catch_weight_RF1_sql
    ))
    # Retrieves the landing total weight
    trip_weight_capacity_sql <- paste(
      readLines(file.path("sql", "trip_weight_vessel_capacity.sql")),
      collapse = "\n"
    )
    trip_weight_capacity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_weight_capacity_sql,
      select_item = DBI::SQL(paste(select_trip_tide_sql,
                                   collapse = ", "
      ))
    )
    trip_weight_capacity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_weight_capacity_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE && is.data.frame(data_connection[[3]]) == TRUE) {
      tide_id_data <- data_connection[[1]]
      catch_weight_RF1_data <- data_connection[[2]]
      trip_weight_capacity_data <- data_connection[[3]]
      select <- tide_id_data$trip_id
      nrow_first <- nrow(tide_id_data)
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
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  catch_weight_RF1_data <- catch_weight_RF1_data %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge data
  tide_id_data <- merge(tide_id_data, catch_weight_RF1_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  tide_id_data <- merge(tide_id_data, trip_weight_capacity_data, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Creation of a tide identification number
  tide_id_data$tide_id <- paste0(tide_id_data$trip_end_tide_id, "_", tide_id_data$trip_previous_end_tide_id)
  # RF1 calculation
  tide_id_data_RF1 <- tide_id_data %>%
    dplyr::group_by(tide_id) %>%
    dplyr::summarise(RF1 = ifelse(all(is.na(trip_landingtotalweight)), trip_landingtotalweight[NA_integer_], sum(trip_landingtotalweight, na.rm = TRUE)) / ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)), tide_landingtotalweight = ifelse(all(is.na(trip_landingtotalweight)), trip_landingtotalweight[NA_integer_], sum(trip_landingtotalweight, na.rm = TRUE)), tide_sum_catch_weight = ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)))
  tide_id_data$lower_limit <- limit[1]
  tide_id_data$upper_limit <- limit[2]
  # Selection of user-supplied trips
  tide_id_data <- merge(data.frame(trip_id = select), unique(tide_id_data), by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Merge data
  tide_id_data <- merge(tide_id_data, tide_id_data_RF1, by.x = "tide_id", by.y = "tide_id", all.x = TRUE)
  # Compare RF1 to valid limits
  comparison_less <- vector_comparison(
    first_vector = tide_id_data$RF1,
    second_vector = tide_id_data$upper_limit,
    comparison_type = "less",
    output = "report"
  )
  comparison_greater <- vector_comparison(
    first_vector = tide_id_data$RF1,
    second_vector = tide_id_data$lower_limit,
    comparison_type = "greater",
    output = "report"
  )
  tide_id_data$logical <- comparison_less$logical & comparison_greater$logical
  # Corrects missing RF1s when nothing has been landed and there is no capture
  tide_id_data[(is.na(tide_id_data$tide_landingtotalweight) | tide_id_data$tide_landingtotalweight == 0) & is.na(tide_id_data$tide_sum_catch_weight),"logical"]<-TRUE
  # Correction of complete tides not yet finished
  tide_id_data[(is.na(tide_id_data$trip_end_tide_id)),"logical"]<-TRUE
  tide_id_data <- dplyr::relocate(.data = tide_id_data, RF1, .after = logical)
  trip_end_tide_id<-tide_id_data$trip_end_tide_id
  tide_id_data <- subset(tide_id_data, select = -c(tide_id, trip_end_tide_id, trip_previous_end_tide_id, vessel_capacity, sum_catch_weight, trip_landingtotalweight, tide_landingtotalweight,tide_sum_catch_weight, trip_localmarkettotalweight, lower_limit, upper_limit))
  if ((sum(tide_id_data$logical) + sum(!tide_id_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="trip"){text_object_more_or_less(select,tide_id_data$trip_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!tide_id_data$logical), " trips with RF1 outside defined thresholds or missing")))
  }
  if (output == "report") {
    return(tide_id_data)
  }
  if (output == "logical") {
    if (sum(!tide_id_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if the school type is consistent with the association, in the future integrated in the pakage codama
check_fishing_context_inspector <- function(data_connection,
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("activity", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("activity", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "activity" &&
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
    # Activity selection in the SQL query
    if (type_select == "activity") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Activity with date in the selected year
      activity_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "activity_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      activity_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = activity_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      activity_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = activity_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", activity_id_selected_by_year_data$activity_id, "'")
    }
    # Retrieves the schooltype of activity
    activity_schooltype_sql <- paste(
      readLines(file.path("sql", "activity_schooltype.sql")),
      collapse = "\n"
    )
    activity_schooltype_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_schooltype_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_schooltype_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_schooltype_sql
    ))
    # Retrieves activities with object-type associations
    activity_association_object_sql <- paste(
      readLines(file.path("sql", "activity_association_object.sql")),
      collapse = "\n"
    )
    activity_association_object_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_association_object_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_association_object_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_association_object_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      activity_schooltype_data <- data_connection[[1]]
      activity_association_object_data <- data_connection[[2]]
      nrow_first <- nrow(activity_schooltype_data)
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
  # Calculates the number of object-type associations
  activity_association_object_data <- activity_association_object_data %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(association_object_count = dplyr::n())
  # Merge
  activity_schooltype_data <- merge(activity_schooltype_data, activity_association_object_data, all.x = TRUE)
  activity_schooltype_data$seuil <- 0
  activity_schooltype_data$association_object_count[is.na(activity_schooltype_data$association_object_count)] <- 0
  # Indicates whether or not an object-type association exists
  comparison <- vector_comparison(
    first_vector = activity_schooltype_data$association_object_count,
    second_vector = activity_schooltype_data$seuil,
    comparison_type = "greater",
    output = "report"
  )
  activity_schooltype_data$logical <- comparison$logical
  # Case of free school : must not have any object-type association (inverse of the result obtained)
  activity_schooltype_data$logical[!is.na(activity_schooltype_data$schooltype_code) & activity_schooltype_data$schooltype_code == "2"] <- !activity_schooltype_data$logical[!is.na(activity_schooltype_data$schooltype_code) & activity_schooltype_data$schooltype_code == "2"]
  # Unknown benches and NA: no constraint
  activity_schooltype_data$logical[is.na(activity_schooltype_data$schooltype_code) | activity_schooltype_data$schooltype_code == "0"] <- TRUE
  activity_schooltype_data <- dplyr::relocate(.data = activity_schooltype_data, schooltype_code, association_object_count, .after = logical)
  activity_schooltype_data <- subset(activity_schooltype_data, select = -c(seuil))
  if ((sum(activity_schooltype_data$logical) + sum(!activity_schooltype_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="activity"){text_object_more_or_less(select,activity_schooltype_data$activity_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!activity_schooltype_data$logical), " activity with school types that do not correspond to the observed associations")))
  }
  if (output == "report") {
    return(activity_schooltype_data)
  }
  if (output == "logical") {
    if (sum(!activity_schooltype_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if succes status is consistent with vessel activity, the type of school or the weight caught, in the future integrated in the pakage codama
check_operationt_inspector <- function(data_connection,
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("activity", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("activity", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "activity" &&
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
    # Activity selection in the SQL query
    if (type_select == "activity") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Activity with date in the selected year
      activity_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "activity_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      activity_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = activity_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      activity_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = activity_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", activity_id_selected_by_year_data$activity_id, "'")
    }
    # Retrieves the schooltype of activity
    activity_sql <- paste(
      readLines(file.path("sql", "activity_schooltype_successstatus_weight_vesselactivity.sql")),
      collapse = "\n"
    )
    activity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      activity_data <- data_connection[[1]]
      nrow_first <- nrow(activity_data)
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
  # Indicates whether the vessel activity requires a success status
  activity_data$seuil <- "6"
  comparison_successstatus_vesselactivity <- vector_comparison(
    first_vector = activity_data$vesselactivity_code,
    second_vector = activity_data$seuil,
    comparison_type = "equal",
    output = "report"
  )
  activity_data$logical_successstatus_vesselactivity <- comparison_successstatus_vesselactivity$logical
  # Case of success status NA: must not have activity 6 (inverse of the result obtained)
  activity_data$logical_successstatus_vesselactivity[is.na(activity_data$successstatus_code)] <- !activity_data$logical_successstatus_vesselactivity[is.na(activity_data$successstatus_code)]
  # Indicates indeterminate school must not have positive or negative success status
  activity_data$seuil <- "0"
  logical_successstatus_schooltype_indeterminate <- vector_comparison(
    first_vector = activity_data$schooltype_code,
    second_vector = activity_data$seuil,
    comparison_type = "difference",
    output = "report"
  )
  activity_data$logical_successstatus_schooltype_indeterminate <- !logical_successstatus_schooltype_indeterminate$logical
  # Case of success status indeterminate or NA: no constraints
  activity_data$logical_successstatus_schooltype_indeterminate[is.na(activity_data$successstatus_code) | activity_data$successstatus_code == "2"] <- TRUE
  # Indicates whether the success status requires a school type 
  activity_data$logical_successstatus_schooltype<-!is.na(activity_data$schooltype_code) 
  # Case of success status NA: no constraints
  activity_data$logical_successstatus_schooltype[is.na(activity_data$successstatus_code)] <- TRUE
  # Indicates whether captured weight is greater than 0
  activity_data$seuil <- 0
  comparison_successstatus_weight <- vector_comparison(
    first_vector = activity_data$activity_weight,
    second_vector = activity_data$seuil,
    comparison_type = "greater",
    output = "report"
  )
  activity_data$logical_successstatus_weight <- comparison_successstatus_weight$logical
  # Case of success status null : must not have weight (inverse of the result obtained)
  activity_data$logical_successstatus_weight[!is.na(activity_data$successstatus_code) & activity_data$successstatus_code == "0"] <- !activity_data$logical_successstatus_weight[!is.na(activity_data$successstatus_code) & activity_data$successstatus_code == "0"]
  # NA success status: no constraints
  activity_data$logical_successstatus_weight[is.na(activity_data$successstatus_code)] <- TRUE
  # NA weight: no constraints
  activity_data$logical_successstatus_weight[is.na(activity_data$activity_weight)] <- TRUE
  # Combines test results
  activity_data$logical <- activity_data$logical_successstatus_vesselactivity & activity_data$logical_successstatus_schooltype_indeterminate & activity_data$logical_successstatus_schooltype & activity_data$logical_successstatus_weight
  activity_data <- dplyr::relocate(.data = activity_data, vesselactivity_code, successstatus_code, schooltype_code, activity_weight, .after = logical)
  activity_data <- subset(activity_data, select = -c(seuil, logical_successstatus_vesselactivity, logical_successstatus_schooltype_indeterminate, logical_successstatus_schooltype, logical_successstatus_weight))
  if ((sum(activity_data$logical) + sum(!activity_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="activity"){text_object_more_or_less(select,activity_data$activity_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!activity_data$logical), " activity with a succes status that doesn't match either the vessel activity, the type of school or the weight caught.")))
  }
  if (output == "report") {
    return(activity_data)
  }
  if (output == "logical") {
    if (sum(!activity_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that tests if ocean declaration is consistent with activity position, in the future integrated in the pakage codama
check_position_inspector <- function(data_connection,
                                     type_select,
                                     select,
                                     output,
                                     ocean_name_nonpriority = "Atlantic") {
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("activity", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("activity", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "activity" &&
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
    if (r_type_checking(
      r_object = ocean_name_nonpriority,
      type = "character",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = ocean_name_nonpriority,
        type = "character",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Activity selection in the SQL query
    if (type_select == "activity") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Activity with date in the selected year
      activity_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "activity_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      activity_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = activity_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      activity_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = activity_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", activity_id_selected_by_year_data$activity_id, "'")
    }
    # Retrieves the position of activity
    activity_sea_land_sql <- paste(
      readLines(file.path("sql", "activity_sea_land.sql")),
      collapse = "\n"
    )
    activity_sea_land_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_sea_land_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_sea_land_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_sea_land_sql
    ))
    # Retrieves the position of activity in harbour
    activity_harbour_sql <- paste(
      readLines(file.path("sql", "activity_harbour.sql")),
      collapse = "\n"
    )
    activity_harbour_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_harbour_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_harbour_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_harbour_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      activity_sea_land_data <- data_connection[[1]]
      activity_harbour_data <- data_connection[[2]]
      nrow_first <- nrow(activity_schooltype_data)
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
  # Indicates whether the ocean is the same
  comparison_ocean <- vector_comparison(
    first_vector = activity_sea_land_data$ocean_name,
    second_vector = activity_sea_land_data$ocean_name_pos,
    comparison_type = "equal",
    output = "report"
  )
  activity_sea_land_data$logical_ocean <- comparison_ocean$logical
  # Indicates whether in land harbour
  comparison_harbour <- vector_comparison(
    first_vector = activity_sea_land_data$activity_id,
    second_vector = activity_harbour_data$activity_id,
    comparison_type = "difference",
    output = "report"
  )
  activity_sea_land_data$logical_harbour <- comparison_harbour$logical
  # Case of harbour in sea : not in harbour
  activity_sea_land_data$logical_harbour[!is.na(activity_sea_land_data$ocean_name_pos)] <- FALSE
  activity_sea_land_data$logical <- activity_sea_land_data$logical_ocean | activity_sea_land_data$logical_harbour
  # Case case where the position is exactly on the boundary of the two oceans: focus on the Indian Ocean
  count_ocean_activity <- activity_sea_land_data %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(count_ocean = length(unique(ocean_name_pos))) %>%
    dplyr::filter(count_ocean == 2)
  activity_sea_land_data <- activity_sea_land_data[!(activity_sea_land_data$activity_id %in% count_ocean_activity$activity_id & activity_sea_land_data$ocean_name_pos == ocean_name_nonpriority), ]
  # Gives the type of location
  activity_sea_land_data$type <- "Sea"
  activity_sea_land_data$type[is.na(activity_sea_land_data$ocean_name_pos)] <- "Land"
  activity_sea_land_data$type[activity_sea_land_data$logical_harbour] <- "Harbour"
  # Case of ocean trip is null :
  activity_sea_land_data$logical[is.na(activity_sea_land_data$ocean_name)] <- FALSE
  activity_sea_land_data <- dplyr::relocate(.data = activity_sea_land_data, type, ocean_name, ocean_name_pos, .after = logical)
  activity_sea_land_data_detail <- activity_sea_land_data
  activity_sea_land_data <- subset(activity_sea_land_data, select = -c(activity_position, activity_crs, logical_ocean, logical_harbour))
  if ((sum(activity_sea_land_data$logical) + sum(!activity_sea_land_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="activity"){text_object_more_or_less(select,activity_sea_land_data$activity_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!activity_sea_land_data$logical), " activity with school types that do not correspond to the observed associations")))
  }
  if (output == "report") {
    return(list(activity_sea_land_data, activity_sea_land_data_detail))
  }
  if (output == "logical") {
    if (sum(!activity_sea_land_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that sum of the weight indicated for the catch is consistent with activity weight, in the future integrated in the pakage codama
check_weight_inspector <- function(data_connection,
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("activity", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("activity", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "activity" &&
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
    # Activity selection in the SQL query
    if (type_select == "activity") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Activity with date in the selected year
      activity_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "activity_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      activity_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = activity_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      activity_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = activity_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", activity_id_selected_by_year_data$activity_id, "'")
    }
    # Retrieves the weigth catch of the activity
    activity_weight_sql <- paste(
      readLines(file.path("sql", "activity_weight.sql")),
      collapse = "\n"
    )
    activity_weight_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_weight_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_weight_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_weight_sql
    ))
    # Retrieves the weight of each capture of the activity
    catch_weight_sql <- paste(
      readLines(file.path("sql", "catch_weight.sql")),
      collapse = "\n"
    )
    catch_weight_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = catch_weight_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    catch_weight_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = catch_weight_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      activity_weight_data <- data_connection[[1]]
      catch_weight_data <- data_connection[[2]]
      nrow_first <- nrow(activity_schooltype_data)
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
  # Calculate the sum of the weight per activity (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  catch_weight_data <- catch_weight_data %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Group the pair to compare
  activity_weight_data <- merge(activity_weight_data, catch_weight_data, by = "activity_id", all.x = TRUE)
  # Compare weight of the activity or the sum of the catch
  comparison <- vector_comparison(
    first_vector = activity_weight_data$activity_weight,
    second_vector = activity_weight_data$sum_catch_weight,
    comparison_type = "equal",
    output = "report"
  )
  # Modify the table for display purposes: add, remove and order column
  activity_weight_data <- cbind(activity_weight_data, comparison)
  activity_weight_data <- dplyr::relocate(.data = activity_weight_data, activity_weight, sum_catch_weight, .after = logical)
  activity_weight_data <- subset(activity_weight_data, select = -c(first_vector, second_vector))
  # Management of the NA value for the weight activity and catch
  activity_weight_data[is.na(activity_weight_data$activity_weight) & is.na(activity_weight_data$sum_catch_weight), "logical"] <- TRUE
  # Management of the 0 value for the weight activity
  activity_weight_data[!is.na(activity_weight_data$activity_weight) & activity_weight_data$activity_weight == 0 & is.na(activity_weight_data$sum_catch_weight), "logical"] <- TRUE
  if ((sum(activity_weight_data$logical) + sum(!activity_weight_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="activity"){text_object_more_or_less(select,activity_weight_data$activity_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!activity_weight_data$logical), " activity with a weight different from the sum of the weight of each catch")))
  }
  if (output == "report") {
    return(activity_weight_data)
  }
  if (output == "logical") {
    if (sum(!activity_weight_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function that size class of the samples depending on the species and measurement type is consistent with valid limits, in the future integrated in the pakage codama
check_length_class_inspector <- function(data_connection,
                                         type_select,
                                         select,
                                         output,
                                         species = c("YFT", "BET", "ALB"),
                                         size_measure_type="FL",
                                         limit=80) {
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("sample", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("sample", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "sample" &&
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
    # Sample selection in the SQL query
    if (type_select == "sample") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Sample with date in the selected year
      sample_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "sample_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      sample_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = sample_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      sample_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = sample_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", sample_id_selected_by_year_data$sample_id, "'")
    }
    # Retrieves the species, measurement type and size class of the samples
    samplespeciesmeasure_sizeclass_sql <- paste(
      readLines(file.path("sql", "samplespeciesmeasure_sizeclass.sql")),
      collapse = "\n"
    )
    samplespeciesmeasure_sizeclass_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = samplespeciesmeasure_sizeclass_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    samplespeciesmeasure_sizeclass_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = samplespeciesmeasure_sizeclass_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection) == TRUE) {
      samplespeciesmeasure_sizeclass_data <- data_connection
      nrow_first <- nrow(samplespeciesmeasure_sizeclass_data)
    } else {
      warning(
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
  samplespeciesmeasure_sizeclass_data$seuil<-limit
  # Compare size class of the samples
  comparison_sizeclass <- vector_comparison(
    first_vector = samplespeciesmeasure_sizeclass_data$samplespeciesmeasure_sizeclass,
    second_vector = samplespeciesmeasure_sizeclass_data$seuil,
    comparison_type = "less_equal",
    output = "report"
  )
  samplespeciesmeasure_sizeclass_data$logical_sizeclass  <- comparison_sizeclass$logical
  # Compare specie of the samples
  comparison_species <- vector_comparison(
    first_vector = samplespeciesmeasure_sizeclass_data$specie_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  samplespeciesmeasure_sizeclass_data$logical_species <- comparison_species$logical
  # Compare size measure type of the samples
  comparison_sizemeasuretype <- vector_comparison(
    first_vector = samplespeciesmeasure_sizeclass_data$sizemeasuretype_code,
    second_vector = size_measure_type,
    comparison_type = "difference",
    output = "report"
  )
  samplespeciesmeasure_sizeclass_data$logical_sizemeasuretype <- comparison_sizemeasuretype$logical
  samplespeciesmeasure_sizeclass_data$logical <- samplespeciesmeasure_sizeclass_data$logical_sizeclass | !samplespeciesmeasure_sizeclass_data$logical_sizemeasuretype | !samplespeciesmeasure_sizeclass_data$logical_species
  # Modify the table for display purposes: add, remove and order column
  samplespeciesmeasure_sizeclass_data <- dplyr::relocate(.data = samplespeciesmeasure_sizeclass_data, specie_code, sizemeasuretype_code, samplespeciesmeasure_sizeclass, .after = logical)
  samplespeciesmeasure_sizeclass_data <- subset(samplespeciesmeasure_sizeclass_data, select = -c(logical_sizeclass, logical_sizemeasuretype, logical_species, seuil, specie_code, sizemeasuretype_code))
  if ((sum(samplespeciesmeasure_sizeclass_data$logical) + sum(!samplespeciesmeasure_sizeclass_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="sample"){text_object_more_or_less(select,samplespeciesmeasure_sizeclass_data$samplespeciesmeasure_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!samplespeciesmeasure_sizeclass_data$logical), " samples with measurements ", paste0(size_measure_type, collapse = ", "), ", for species ", paste0(species, collapse = ", "), ", greater than ", limit)))
  }
  if (output == "report") {
    return(samplespeciesmeasure_sizeclass_data)
  }
  if (output == "logical") {
    if (sum(!samplespeciesmeasure_sizeclass_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function the total number of individuals measured per sample is consistent with the sum of individuals per sample, species and size class, in the future integrated in the pakage codama
check_measure_inspector  <- function(data_connection,
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
  if (any(grep("observe_",data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("sample", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("sample", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "sample" &&
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
    if (type_select == "sample") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Trip with a departure or arrival date in the selected year
      sample_id_selected_by_year_sql <- paste(
        readLines(con = system.file("sql",
                                    "sample_id_selected_by_year.sql",
                                    package = "codama"
        )),
        collapse = "\n"
      )
      sample_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = sample_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      sample_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = sample_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", sample_id_selected_by_year_data$trip_id, "'")
    }
    # number of individual measurements by sample and by species
    samplespecies_measuredcount_sql <- paste(
      readLines(file.path("sql", "samplespecies_measuredcount.sql")),
      collapse = "\n"
    )
    samplespecies_measuredcount_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = samplespecies_measuredcount_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    samplespecies_measuredcount_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = samplespecies_measuredcount_sql
    ))
    # number of individual measurements by sample and by species and by size class
    samplespeciesmeasure_count_sql <- paste(
      readLines(file.path("sql", "samplespeciesmeasure_count.sql")),
      collapse = "\n"
    )
    samplespeciesmeasure_count_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = samplespeciesmeasure_count_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    samplespeciesmeasure_count_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = samplespeciesmeasure_count_sql
    ))
    nrow_first<-length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      samplespecies_measuredcount_data <- data_connection[[1]]
      samplespeciesmeasure_count_data <- data_connection[[2]]
      nrow_first <- nrow(trip_weight_capacity_data)
    } else {
      warning(
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
  # Calculates the total sum of individuals by sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  samplespecies_measuredcount_data <- samplespecies_measuredcount_data %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_measuredcount = ifelse(all(is.na(samplespecies_measuredcount)), samplespecies_measuredcount[NA_integer_], sum(samplespecies_measuredcount, na.rm = TRUE)))
  samplespeciesmeasure_count_data <- samplespeciesmeasure_count_data %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_count = ifelse(all(is.na(samplespeciesmeasure_count)), samplespeciesmeasure_count[NA_integer_], sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Merge
  samplespecies_measuredcount_data <- merge(samplespecies_measuredcount_data, samplespeciesmeasure_count_data, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)
  # Compare the two sums
  comparison <- vector_comparison(
    first_vector = samplespecies_measuredcount_data$sum_measuredcount,
    second_vector = samplespecies_measuredcount_data$sum_count,
    comparison_type = "equal",
    output = "report"
  )
  samplespecies_measuredcount_data$logical <- comparison$logical
  samplespecies_measuredcount_data <- dplyr::relocate(.data = samplespecies_measuredcount_data, sum_measuredcount, sum_count, .after = logical)
  # Management of missing count measurements by sample and by species
  samplespecies_measuredcount_data[is.na(samplespecies_measuredcount_data$sum_measuredcount), "logical"] <- FALSE
  # Management of missing count measurements by sample and by species and by size class
  samplespecies_measuredcount_data[is.na(samplespecies_measuredcount_data$sum_count), "logical"] <- FALSE
  if ((sum(samplespecies_measuredcount_data$logical) + sum(!samplespecies_measuredcount_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="sample"){text_object_more_or_less(select,samplespecies_measuredcount_data$sample_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!samplespecies_measuredcount_data$logical), " samples with number of individuals measured per species different from number measured per species and size class")))
  }
  if (output == "report") {
    return(samplespecies_measuredcount_data)
  }
  if (output == "logical") {
    if (sum(!samplespecies_measuredcount_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


# Function the sea surface temperature is consistent with valid limits, in the future integrated in the pakage codama
check_temperature_inspector <- function(data_connection,
                                        type_select,
                                        select,
                                        output,
                                        limit = c(15, 32)) {
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("activity", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("activity", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "activity" &&
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
    # Activity selection in the SQL query
    if (type_select == "activity") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Activity with date in the selected year
      activity_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "activity_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      activity_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = activity_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      activity_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = activity_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", activity_id_selected_by_year_data$activity_id, "'")
    }
    # Retrieves the sea surface temperature of the activity
    activity_seasurfacetemperature_sql <- paste(
      readLines(file.path("sql", "activity_seasurfacetemperature.sql")),
      collapse = "\n"
    )
    activity_seasurfacetemperature_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = activity_seasurfacetemperature_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    activity_seasurfacetemperature_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = activity_seasurfacetemperature_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE) {
      activity_seasurfacetemperature_data <- data_connection
      nrow_first <- nrow(activity_seasurfacetemperature_data)
    } else {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Consistency check not developed yet for this \"data_connection\" argument, you can provide the datasets instead.\n ",
        sep = ""
      )
    }
  }
  # 3 - Data design ----
  # Compare RF1 to valid limits
  activity_seasurfacetemperature_data$lower_limit <- limit[1]
  activity_seasurfacetemperature_data$upper_limit <- limit[2]
  comparison_less <- vector_comparison(
    first_vector = activity_seasurfacetemperature_data$activity_seasurfacetemperature,
    second_vector = activity_seasurfacetemperature_data$upper_limit,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_greater <- vector_comparison(
    first_vector = activity_seasurfacetemperature_data$activity_seasurfacetemperature,
    second_vector = activity_seasurfacetemperature_data$lower_limit,
    comparison_type = "greater_equal",
    output = "report"
  )
  activity_seasurfacetemperature_data$logical <- comparison_less$logical & comparison_greater$logical
  # Management of the NA value for the sea surface temperature
  activity_seasurfacetemperature_data[is.na(activity_seasurfacetemperature_data$activity_seasurfacetemperature), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  activity_seasurfacetemperature_data <- dplyr::relocate(.data = activity_seasurfacetemperature_data, activity_seasurfacetemperature, .after = logical)
  activity_seasurfacetemperature_data <- subset(activity_seasurfacetemperature_data, select = -c(lower_limit, upper_limit))
  if ((sum(activity_seasurfacetemperature_data$logical) + sum(!activity_seasurfacetemperature_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="activity"){text_object_more_or_less(select,activity_seasurfacetemperature_data$activity_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!activity_seasurfacetemperature_data$logical), " activity with sea surface temperature outside defined thresholds")))
  }
  if (output == "report") {
    return(activity_seasurfacetemperature_data)
  }
  if (output == "logical") {
    if (sum(!activity_seasurfacetemperature_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function the species sampled is consistent with species authorized, in the future integrated in the pakage codama
check_species_inspector <- function(data_connection,
                                    type_select,
                                    select,
                                    output,
                                    species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT")) {
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
  if (any(grep("observe_", data_connection[1]))) {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("sample", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("sample", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "sample" &&
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
    # Sample selection in the SQL query
    if (type_select == "sample") {
      select_sql <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      # Sample with date in the selected year
      sample_id_selected_by_year_sql <- paste(
        readLines(file.path(
          "sql",
          "sample_id_selected_by_year.sql"
        )),
        collapse = "\n"
      )
      sample_id_selected_by_year_sql <- DBI::sqlInterpolate(
        conn = data_connection[[2]],
        sql = sample_id_selected_by_year_sql,
        select_item = DBI::SQL(paste(select,
                                     collapse = ", "
        ))
      )
      sample_id_selected_by_year_data <- dplyr::tibble(DBI::dbGetQuery(
        conn = data_connection[[2]],
        statement = sample_id_selected_by_year_sql
      ))
      select_sql <- paste0("'", sample_id_selected_by_year_data$sample_id, "'")
    }
    # Retrieves the species of the samples
    samplespecies_specie_sql <- paste(
      readLines(file.path("sql", "samplespecies_specie.sql")),
      collapse = "\n"
    )
    samplespecies_specie_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = samplespecies_specie_sql,
      select_item = DBI::SQL(paste(select_sql,
                                   collapse = ", "
      ))
    )
    samplespecies_specie_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = samplespecies_specie_sql
    ))
    nrow_first <- length(unique(select_sql))
  } else {
    if (is.data.frame(data_connection) == TRUE) {
      samplespecies_specie_data <- data_connection
      nrow_first <- nrow(samplespecies_specie_data)
    } else {
      warning(
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
  # Compare specie of the samples
  comparison_species <- vector_comparison(
    first_vector = samplespecies_specie_data$specie_name,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  samplespecies_specie_data$logical <- comparison_species$logical
  # Modify the table for display purposes: add, remove and order column
  samplespecies_specie_data <- dplyr::relocate(.data = samplespecies_specie_data, specie_name, .after = logical)
  if ((sum(samplespecies_specie_data$logical) + sum(!samplespecies_specie_data$logical)) != nrow_first) {
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      if(type_select=="sample"){text_object_more_or_less(select,samplespecies_specie_data$samplespecies_id)},
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!samplespecies_specie_data$logical), " samples with species not included in the authorized list (",paste0(species, collapse = ", "),")", collapse = ", ")))
  }
  if (output == "report") {
    return(samplespecies_specie_data)
  }
  if (output == "logical") {
    if (sum(!samplespecies_specie_data$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# Function the sample is consistent with the presence of measurement, in the future integrated in the pakage codama
check_sample_without_measure_inspector <- function(dataframe1,
                                                   output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespeciesmeasure_id"),
    column_type = c("character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespeciesmeasure_id"),
      column_type = c("character", "character"),
      output = "message"
    )
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
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search samplespeciesmeasure ID in the associations samplespecies ID
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(samplespecies_id) %>%
    dplyr::summarise(count = sum(!is.na(samplespeciesmeasure_id)))
  dataframe1$logical <- TRUE
  dataframe1[dataframe1$count == 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(count))
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample is consistent with the presence of species, in the future integrated in the pakage codama
check_sample_without_species_inspector <- function(dataframe1,
                                                   output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "samplespecies_id"),
    column_type = c("character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "samplespecies_id"),
      column_type = c("character", "character"),
      output = "message"
    )
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search samplespecies ID in the associations samples ID
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count = sum(!is.na(samplespecies_id)))
  dataframe1$logical <- TRUE
  dataframe1[dataframe1$count == 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(count))
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample is consistent with the subsample number, in the future integrated in the pakage codama
check_super_sample_number_consistent_inspector <- function(dataframe1,
                                                           dataframe2,
                                                           output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_supersample"),
    column_type = c("character", "logical"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_supersample"),
      column_type = c("character", "logical"),
      output = "message"
    )
  }else {
    dataframe1<-dataframe1[,c("sample_id", "sample_supersample")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "message"
    )
  }else{
    dataframe2<-dataframe2[,c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id")]
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search subsample number in the associations samples ID
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count_subsamplenumber_N0 = sum(samplespecies_subsamplenumber != 0), count_subsamplenumber_0 = sum(samplespecies_subsamplenumber == 0), count_samplespecies = sum(!is.na(unique(samplespecies_id))), count_subsamplenumber_1 = sum(samplespecies_subsamplenumber == 1))
  # Merge
  dataframe1$logical <- TRUE
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  # Case of NA count_subsamplenumber_N0, count_subsamplenumber_0, count_samplespecies or count_subsamplenumber_1
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      count_subsamplenumber_N0_bis = dplyr::coalesce(count_subsamplenumber_N0, 0),
      count_subsamplenumber_0_bis = dplyr::coalesce(count_subsamplenumber_0, 0),
      count_samplespecies_bis = dplyr::coalesce(count_samplespecies, 0),
      count_subsamplenumber_1_bis = dplyr::coalesce(count_subsamplenumber_1, 0),
    )
  dataframe1[dataframe1$count_samplespecies_bis == 0, "logical"] <- FALSE
  dataframe1$only_one_subsampling <- dataframe1$sample_supersample == FALSE & dataframe1$count_subsamplenumber_N0_bis == 0
  dataframe1$many_subsampling <- dataframe1$sample_supersample == TRUE & dataframe1$count_subsamplenumber_0_bis == 0 & dataframe1$count_samplespecies_bis > 1
  dataframe1[!(dataframe1$only_one_subsampling | dataframe1$many_subsampling), "logical"] <- FALSE
  dataframe1[dataframe1$count_samplespecies_bis == 1 & dataframe1$count_subsamplenumber_1_bis > 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(only_one_subsampling, many_subsampling, count_samplespecies_bis, count_subsamplenumber_N0_bis, count_subsamplenumber_0_bis, count_subsamplenumber_1_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_supersample, count_subsamplenumber_N0, count_subsamplenumber_0, count_subsamplenumber_1, count_samplespecies, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample well number is consistent with the associated trip well numbers, in the future integrated in the pakage codama
check_well_number_consistent_inspector <- function(dataframe1,
                                                   dataframe2,
                                                   output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_well", "trip_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_well", "trip_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "well_well"),
    column_type = c("character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "well_well"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "well_well")]
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # merge
  dataframe2$logical <- TRUE
  dataframe1 <- merge(dataframe1, dataframe2, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_well"), all.x = TRUE)
  # Search well not link
  dataframe1[is.na(dataframe1$logical), "logical"] <- FALSE
  # Case the well number is empty
  dataframe1[is.na(dataframe1$sample_well), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_well, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample is consistent for the percentage of little and big fish sampled, in the future integrated in the pakage codama
check_little_big_inspector <- function(dataframe1,
                                       dataframe2,
                                       dataframe3,
                                       output,
                                       species = c("YFT", "YFT", "BET", "BET", "ALB", "ALB"),
                                       measuretype = c("PD1", "FL", "PD1", "FL", "PD1", "FL"),
                                       sizelimit = c(24, 80, 24, 77, 23.5, 78),
                                       measuretype_size = c("FL", "PD1"),
                                       threshold = 0.9) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"), output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id")]
  }
  if (r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id"),
    column_type = c("character", "numeric", "numeric", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
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
  if (r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = species,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = measuretype,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = measuretype,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = sizelimit,
    type = "numeric",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = sizelimit,
      type = "numeric",
      output = "message"
    ))
  }
  if (length(species) != length(measuretype) | length(species) != length(sizelimit)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"species\", \"measuretype\" and \"sizelimit\"\n"
    )
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  if (r_type_checking(
    r_object = measuretype_size,
    type = "character",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = measuretype_size,
      type = "character",
      length = 2L,
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "message"
    ))
  }
  # 2 - Data design ----
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe3, by = "samplespecies_id", all.x = TRUE)
  # Calculate the number of the measure per sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  total_count <- dataframe1 %>%
    dplyr::group_by(sample_id, sample_smallsweight, sample_bigsweight, sample_totalweight) %>%
    dplyr::summarise(total_count = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)), .groups = "keep")
  # Small and large category conditions
  condition <- as.list(as.data.frame(t(data.frame(species, measuretype, sizelimit))))
  # Measurement selection of small individuals
  little <- purrr::map(condition, ~ dataframe1 %>%
                         dplyr::filter(specie_name == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass < as.numeric(.x[3])))
  little <- do.call(rbind.data.frame, little)
  # Calculation of the number of measurements of small individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  little <- little %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(little = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Measurement selection of big individuals
  big <- purrr::map(condition, ~ dataframe1 %>%
                      dplyr::filter(specie_name == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass >= as.numeric(.x[3])))
  big <- do.call(rbind.data.frame, big)
  # Calculation of the number of measurements of big individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  big <- big %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(big = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 1 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == measuretype_size[1]) %>%
    dplyr::summarise(measure1 = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 2 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure2 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == measuretype_size[2]) %>%
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
   total_count[is.na(total_count$little_percentage), "little_percentage"]<-0
   total_count[is.na(total_count$big_percentage), "big_percentage"]<-0
   total_count[is.na(total_count$measure1_percentage), "measure1_percentage"]<-0
   total_count[is.na(total_count$measure2_percentage), "measure2_percentage"]<-0
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
  if ((sum(total_count$logical) + sum(!total_count$logical)) != nrow_first) {
    all <- c(select, total_count$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample is consistent for the weighting, in the future integrated in the pakage codama
check_weighting_inspector <- function(dataframe1,
                                      dataframe2,
                                      dataframe3,
                                      dataframe4,
                                      output,
                                      vessel_type = c("6", "2"), 
                                      weight_limit = 100,
                                      threshold = 0.95,
                                      sampletype_code_landing_baitboat = c("11"),
                                      landingtype_baitboat = c("L-YFT-10", "L-BET-10", "L-TUN-10")) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
    column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
      column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sampleactivity_weightedweight"),
    column_type = c("character", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "sampleactivity_weightedweight"),
      column_type = c("character", "numeric"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "sampleactivity_weightedweight")]
  }
  if (r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("trip_id", "vesseltype_code", "vesseltype_label1"),
    column_type = c("character", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("trip_id", "vesseltype_code", "vesseltype_label1"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code", "vesseltype_label1")]
  }
  if (r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("trip_id", "landing_weight", "weightcategory_code"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
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
  if (r_type_checking(
    r_object = vessel_type,
    type = "character",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = vessel_type,
      type = "character",
      length = 2L,
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = weight_limit,
    type = "numeric",
    length = 1L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = weight_limit,
      type = "numeric",
      length = 1L,
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = landingtype_baitboat,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = landingtype_baitboat,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = sampletype_code_landing_baitboat,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
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
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weight > weight_limit, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weightedweight_bis < dataframe1$weight & !((dataframe1$weightedweight_bis / dataframe1$weight) >= threshold), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & dataframe1$sampletype_code %in% sampletype_code_landing_baitboat & abs(dataframe1$weightedweight_bis - dataframe1$weight) > 1, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & !(dataframe1$sampletype_code %in% sampletype_code_landing_baitboat) & abs(dataframe1$weightedweight_bis - dataframe1$sum_landing_weight_bis) > 1, "logical"] <- FALSE
  # Case NA vesseltype_code sampletype_code
  dataframe1[is.na(dataframe1$vesseltype_code), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & is.na(dataframe1$sampletype_code), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_calculation, weight, vesseltype_code, weightedweight_bis, sum_landing_weight_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_totalweight, sampletype_code, weightedweight, vesseltype_label1, sum_landing_weight, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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


# Function the sample weight (m10 and p10) is consistent for the global weight, in the future integrated in the pakage codama
check_weight_sample_inspector <- function(dataframe1,
                                          output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), NaN, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE)))
  # Check
  comparison_weight_calculation <- vector_comparison(
    first_vector = dataframe1$weight_calculation,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  comparison_totalweight <- vector_comparison(
    first_vector = dataframe1$sample_totalweight,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical<- !(comparison_weight_calculation$logical & comparison_totalweight$logical) & !(is.na(dataframe1$weight_calculation) & is.na(dataframe1$sample_totalweight))
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(weight_calculation))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_totalweight, sample_smallsweight, sample_bigsweight, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample is consistent for the existence of the activity, in the future integrated in the pakage codama
check_activity_sample_inspector <- function(dataframe1,
                                            output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "activity_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "activity_id")]
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation activity_id (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count_activity = ifelse(all(is.na(activity_id)), 0L, sum(!is.na(activity_id))))
  # Check
  comparison <- vector_comparison(
    first_vector = dataframe1$count_activity,
    second_vector = c(0L),
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- !comparison$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, count_activity, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the sample measurement types is consistent for the species or weight values, in the future integrated in the pakage codama
check_ldlf_inspector <- function(dataframe1,
                                 dataframe2,
                                 output,
                                 species = c("SKJ", "LTA", "FRI"),
                                 measuretype_species = c("PD1"),
                                 measuretype_bigsweight = c("PD1"),
                                 measuretype_smallsweight = c("FL")) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "specie_name", "sizemeasuretype_code", "sample_id")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
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
  if (r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = species,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = measuretype_species,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = measuretype_species,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = measuretype_bigsweight,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = measuretype_bigsweight,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = measuretype_smallsweight,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = measuretype_smallsweight,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Check species and measuretype
  comparison_species <- vector_comparison(
    first_vector = dataframe1$specie_name,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  comparison_measuretype_species <- vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = measuretype_species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_species <- !(comparison_species$logical & comparison_measuretype_species$logical)
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2, by = "sample_id", all.x = TRUE)
  # Check bigs weight and measuretype
  comparison_measuretype_bigsweight <- vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = measuretype_bigsweight,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_bigsweight <- !(comparison_measuretype_bigsweight$logical & ((dataframe1$sample_bigsweight == 0 & dataframe1$sample_totalweight == 0) | (is.na(dataframe1$sample_bigsweight) & is.na(dataframe1$sample_totalweight))))
  # Check smalls weight and measuretype
  comparison_measuretype_smallsweight <- vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = measuretype_smallsweight,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_smallsweight <- !(comparison_measuretype_smallsweight$logical & ((dataframe1$sample_smallsweight == 0 & dataframe1$sample_totalweight == 0) | (is.na(dataframe1$sample_smallsweight) & is.na(dataframe1$sample_totalweight))))
  # Check
  dataframe1$logical <- dataframe1$logical_species & dataframe1$logical_bigsweight & dataframe1$logical_smallsweight
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(logical_species, logical_bigsweight, logical_smallsweight, sample_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sizemeasuretype_code, specie_name, sample_bigsweight, sample_smallsweight, sample_totalweight, .after = logical)
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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

# Function the small and large sample weights is consistent for the sum of the small and big weights of the associated well, in the future integrated in the pakage codama
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
  # 1 - Arguments verification ----
  if (r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
    column_type = c("character", "character", "character", "numeric", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
      column_type = c("character", "character", "character", "numeric", "numeric"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight")]
  }
  if (r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("well_id", "well_well", "trip_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("well_id", "well_well", "trip_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("well_id", "well_well", "trip_id")]
  }
  if (r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("wellactivity_id", "well_id"),
    column_type = c("character", "character"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("wellactivity_id", "well_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("wellactivity_id", "well_id")]
  }
  if (r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "specie_name", "wellactivityspecies_weight"),
    column_type = c("character", "character", "character", "character", "numeric"),
    output = "logical"
  ) != TRUE) {
    r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "specie_name", "wellactivityspecies_weight"),
      column_type = c("character", "character", "character", "character", "numeric"),
      output = "message"
    )
  } else {
    dataframe4 <- dataframe4[, c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "specie_name", "wellactivityspecies_weight")]
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
  if (r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = species,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = weightcategory_small,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = weightcategory_small,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = weightcategory_big,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = weightcategory_big,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = weightcategory_unknown,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
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
    dplyr::group_by(well_id, trip_id, well_well) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_small) %>%
    dplyr::reframe(weight_small = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  dataframe2_small_unknown <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_well) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_unknown & specie_name %in% species) %>%
    dplyr::reframe(weight_small_unknown = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  # Calculation big weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2_big <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_well) %>%
    dplyr::filter(weightcategory_code %in% weightcategory_big) %>%
    dplyr::reframe(weight_big = ifelse(all(is.na(wellactivityspecies_weight)), NaN, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::select(-well_id)
  # Merge
  dataframe1 <- merge(dataframe1, dataframe2_small, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_well"), all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe2_small_unknown, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_well"), all.x = TRUE)
  dataframe1 <- merge(dataframe1, dataframe2_big, by.x = c("trip_id", "sample_well"), by.y = c("trip_id", "well_well"), all.x = TRUE)
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
  comparison_smallsweight <- vector_comparison(
    first_vector = dataframe1$weight_small_total_bis,
    second_vector = dataframe1$sample_smallsweight_bis,
    comparison_type = "equal",
    output = "report"
  )
  # Check smalls weight
  comparison_bigsweight <- vector_comparison(
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
  if ((sum(dataframe1$logical) + sum(!dataframe1$logical)) != nrow_first) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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


# Shiny function : Error message if the trip selection elements are not correctly filled in
text_error_trip_select_server <- function(id, parent_in) {
  moduleServer(id, function(input, output, session) {
    text_error_trip_select <- eventReactive(input$start_button, {
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
    config_data <- eventReactive(input$start_button, {
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
    trip_select <- eventReactive(input$start_button, {
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
        # If the database is "observe_", read, transform and execute the SQL query that selects the trips according to the user parameters
        if (any(grep("observe_", data_connection[1]))) {
          # Read the SQL query
          trip_id_sql <- paste(
            readLines(file.path(".", "sql", "trip_id.sql")),
            collapse = "\n"
          )
          # Transform the SQL query
          # Deletes the part linked to the selection with a date range
          if (isTruthy(parent_in$vessel_number) && isTruthy(parent_in$trip_end_date)) {
            trip_id_sql <- sub(
              pattern = "(t.startdate >= ?select_item_2 AND t.enddate <= ?select_item_3)",
              replacement = "",
              x = trip_id_sql,
              fixed = TRUE
            )
            select_item_2 <- parent_in$vessel_number
            select_item_3 <- parent_in$trip_end_date
          }
          # Deletes the part linked to a selection with the vessel code and the end date of the trip
          if (isTruthy(parent_in$trip_start_date_range) && isTruthy(parent_in$trip_end_date_range)) {
            trip_id_sql <- sub(
              pattern = "(v.code IN (?select_item_2) AND t.enddate IN (?select_item_3))",
              replacement = "",
              x = trip_id_sql,
              fixed = TRUE
            )
            select_item_2 <- parent_in$trip_start_date_range
            select_item_3 <- parent_in$trip_end_date_range
          }
          # Replaces the anchors with the selected values
          trip_id_sql <- DBI::sqlInterpolate(
            conn = data_connection[[2]],
            sql = trip_id_sql,
            select_item_1 = DBI::SQL(paste(paste0("'", config_data()[["logbook_program"]], "'"), collapse = ", ")),
            select_item_2 = DBI::SQL(paste0("'", select_item_2, "'")),
            select_item_3 = DBI::SQL(paste0("'", select_item_3, "'"))
          )
          # Execute the SQL query
          trip_id_data <- dplyr::tibble(DBI::dbGetQuery(
            conn = data_connection[[2]],
            statement = trip_id_sql
          ))
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
    calcul_check <- reactive({
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
        # If the database is "observe_", read, transform and execute the SQL query that selects the trips according to the user parameters
        if (any(grep("observe_", data_connection[1]))) {
          # Retrieve trip activity
          # Read the SQL query to retrieve the vessel code, end of the trip, date of th activity and activity number of all the activity that have been selected
          activity_id_sql <- paste(
            readLines(file.path("sql", "activity_id.sql")),
            collapse = "\n"
          )
          # Replaces the anchors with the selected values
          activity_id_sql <- DBI::sqlInterpolate(
            conn = data_connection[[2]],
            sql = activity_id_sql,
            select_item = DBI::SQL(paste(paste0("'", trip_select()$trip_id, "'"), collapse = ", "))
          )
          # Execute the SQL query
          activity_select <- dplyr::tibble(DBI::dbGetQuery(
            conn = data_connection[[2]],
            statement = activity_id_sql
          ))
          # Retrieve trip sample
          # Read the SQL query to retrieve the vessel code, end of the trip and sample number of all the sample that have been selected
          sample_id_sql <- paste(
            readLines(file.path("sql", "sample_id.sql")),
            collapse = "\n"
          )
          # Replaces the anchors with the selected values
          sample_id_sql <- DBI::sqlInterpolate(
            conn = data_connection[[2]],
            sql = sample_id_sql,
            select_item = DBI::SQL(paste(paste0("'", trip_select()$trip_id, "'"), collapse = ", "))
          )
          # Execute the SQL query
          sample_select <- dplyr::tibble(DBI::dbGetQuery(
            conn = data_connection[[2]],
            statement = sample_id_sql
          ))
          # Retrieve trip sample species
          # Read the SQL query to retrieve the vessel code, end of the trip, sample number, species FAO code and type of measure of all the sample that have been selected
          samplespecies_id_sql <- paste(
            readLines(file.path("sql", "samplespecies_id.sql")),
            collapse = "\n"
          )
          # Replaces the anchors with the selected values
          samplespecies_id_sql <- DBI::sqlInterpolate(
            conn = data_connection[[2]],
            sql = samplespecies_id_sql,
            select_item = DBI::SQL(paste(paste0("'", trip_select()$trip_id, "'"), collapse = ", "))
          )
          # Execute the SQL query
          samplespecies_select <- dplyr::tibble(DBI::dbGetQuery(
            conn = data_connection[[2]],
            statement = samplespecies_id_sql
          ))
          # Retrieve trip sample species measure
          # Read the SQL query to retrieve the vessel code, end of the trip, sample number, species FAO code and type of measure of all the sample that have been selected
          samplespeciesmeasure_id_sql <- paste(
            readLines(file.path("sql", "samplespeciesmeasure_id.sql")),
            collapse = "\n"
          )
          # Replaces the anchors with the selected values
          samplespeciesmeasure_id_sql <- DBI::sqlInterpolate(
            conn = data_connection[[2]],
            sql = samplespeciesmeasure_id_sql,
            select_item = DBI::SQL(paste(paste0("'", trip_select()$trip_id, "'"), collapse = ", "))
          )
          # Execute the SQL query
          samplespeciesmeasure_select <- dplyr::tibble(DBI::dbGetQuery(
            conn = data_connection[[2]],
            statement = samplespeciesmeasure_id_sql
          ))
          # Uses a function which indicates whether the selected trips contain activities or not
          check_trip_activity_inspector_data <- check_trip_activity_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report"
          )
          # Uses a function which indicates whether the selected trips contain fishing time inconsistent
          check_fishing_time_inspector_data <- check_fishing_time_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report"
          )
          # Uses a function which indicates whether the selected trips contain sea time inconsistent
          check_sea_time_inspector_data <- check_sea_time_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report"
          )
          # Uses a function which indicates whether the selected trips contain landing total weight inconsistent with vessel capacity
          check_landing_consistent_inspector_data <- check_landing_consistent_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report"
          )
          # Uses a function which indicates whether the selected trips contain the total landed weight for canneries inconsistent with the weights of each landing for the canneries
          check_landing_total_weight_inspector_data <- check_landing_total_weight_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report",
            epsilon = config_data()[["epsilon"]]
          )
          # Uses a function which indicates whether the selected trips contain the trip start and end date inconsistent with the dates of activity
          check_temporal_limit_inspector_data <- check_temporal_limit_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report"
          )
          # Uses a function which indicates whether the selected trips contain the trip harbour of departure of the current trip inconsistent with the harbour of landing of the previous trip
          check_harbour_inspector_data <- check_harbour_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report",
            logbook_program = config_data()[["logbook_program"]]
          )
          # Uses a function which indicates whether the selected trips contain RF1 inconsistent with limit values
          check_raising_factor_inspector_data <- check_raising_factor_inspector(
            data_connection = data_connection,
            type_select = "trip",
            select = trip_select()$trip_id,
            output = "report",
            logbook_program = config_data()[["logbook_program"]],
            species = config_data()[["species_RF1"]]
          )
          # Uses a function which indicates whether the school type is consistent with the association
          check_fishing_context_inspector_data <- check_fishing_context_inspector(
            data_connection = data_connection,
            type_select = "activity",
            select = activity_select$activity_id,
            output = "report"
          )
          # Uses a function which indicates whether the succes status is consistent with the vessel activity, the type of school or the weight caught
          check_operationt_inspector_data <- check_operationt_inspector(
            data_connection = data_connection,
            type_select = "activity",
            select = activity_select$activity_id,
            output = "report"
          )
          # Uses a function which indicates whether the ocean declaration is consistent with activity position
          check_position_inspector_data <- check_position_inspector(
            data_connection = data_connection,
            type_select = "activity",
            select = activity_select$activity_id,
            output = "report"
          )
          # Uses a function which indicates whether that sum of the weight indicated for the catch is consistent with activity weight
          check_weight_inspector_data<-check_weight_inspector(
            data_connection = data_connection,
            type_select = "activity",
            select = activity_select$activity_id,
            output = "report"
          )
          # Uses a function which indicates whether that size class of the samples depending on the species and measurement type is consistent with valid limits
          check_length_class_inspector_data<-check_length_class_inspector(
            data_connection = data_connection,
            type_select = "sample",
            select = samplespeciesmeasure_select$samplespeciesmeasure_id,
            output = "report"
          )
          # Uses a function which indicates whether that total number of individuals measured per sample is consistent with the sum of individuals per sample, species and size class
          check_measure_inspector_data<-check_measure_inspector(
            data_connection = data_connection,
            type_select = "sample",
            select = sample_select$sample_id,
            output = "report"
          )
          # Uses a function which indicates whether that sea surface temperature is consistent with the valid limits
          check_temperature_inspector_data<-check_temperature_inspector(
            data_connection = data_connection,
            type_select = "activity",
            select = activity_select$activity_id,
            output = "report"
          )
          # Uses a function which indicates whether that species sampled is consistent with species authorized
          check_species_inspector_data<-check_species_inspector(
            data_connection = data_connection,
            type_select = "sample",
            select = samplespecies_select$samplespecies_id,
            output = "report"
          )
          # Uses a function to extract data from samplespeciesmeasure in connection with samplespecies
          data_samplespecies_samplespeciesmeasure<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","samplespecies_samplespeciesmeasure.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = samplespecies_select$samplespecies_id))
          # Uses a function to extract data from samplespecies in connection with sample
          data_sample_samplespecies<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","sample_samplespecies.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = sample_select$sample_id))
          # Uses a function to extract data from sample
          data_sample<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","sample.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = sample_select$sample_id))
          # Uses a function to extract data from sample species
          data_samplespecies<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","samplespecies.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = samplespecies_select$samplespecies_id))
          # Uses a function to extract data from well
          data_well<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","well.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = trip_select()$trip_id))
          # Uses a function to extract data from sample species measure
          data_samplespeciesmeasure<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","samplespeciesmeasure.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = samplespeciesmeasure_select$samplespeciesmeasure_id))
          # Uses a function to extract data from landing
          data_landing<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","landing.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = trip_select()$trip_id))
          # Uses a function to extract data from sample activity
          data_sampleactivity<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","sampleactivity.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = sample_select$sample_id))
          # Uses a function to extract data from trip
          data_trip<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","trip.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = trip_select()$trip_id))
          # Uses a function to extract data from activity in connection with sample
          data_sample_activity<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","sample_activity.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = sample_select$sample_id))
          # Uses a function to extract data from wellactivity in connection with trip
          wellactivity_select<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","trip_wellactivity.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = trip_select()$trip_id))
          # Uses a function to extract data from wellactivity
          data_wellactivity<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","wellactivity.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = wellactivity_select$wellactivity_id))
          # Uses a function to extract data from wellactivityspecies
          data_wellactivityspecies<-furdeb::data_extraction(
            type = "database", 
            file_path = file.path("sql","wellactivityspecies.sql"), 
            database_connection = data_connection, 
            anchor = list(select_item = wellactivity_select$wellactivity_id))
          # Disconnection to the bases
          DBI::dbDisconnect(data_connection[[2]])
          # Uses a function to format the table
          check_trip_activity <- table_display_trip(check_trip_activity_inspector_data, trip_select(), type_inconsistency = "warning")
          # Uses a function to format the table
          check_fishing_time <- table_display_trip(check_fishing_time_inspector_data, trip_select(), type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_fishing_time <- dplyr::rename(
            .data = check_fishing_time,
            `Trip fishing time` = trip_fishingtime,
            `Sum route fishing time` = sum_route_fishingtime
          )
          # Uses a function to format the table
          check_sea_time <- table_display_trip(check_sea_time_inspector_data, trip_select(), type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_sea_time <- dplyr::rename(
            .data = check_sea_time,
            `Trip sea time` = trip_seatime,
            `Sum route sea time` = sum_route_seatime
          )
          # Uses a function to format the table
          check_landing_consistent <- table_display_trip(check_landing_consistent_inspector_data, trip_select(), type_inconsistency = "warning")
          # Modify the table for display purposes: rename column
          check_landing_consistent <- dplyr::rename(
            .data = check_landing_consistent,
            `Vessel capacity` = vessel_capacity,
            `Total weight` = trip_weighttotal
          )
          # Uses a function to format the table
          check_landing_total_weigh <- table_display_trip(check_landing_total_weight_inspector_data, trip_select(), type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_landing_total_weigh <- dplyr::rename(
            .data = check_landing_total_weigh,
            `Trip landing weight` = trip_landingtotalweight,
            `Sum landing weight` = sum_weightlanding
          )
          # Data preparation
          check_temporal_limit <- check_temporal_limit_inspector_data[[1]]
          check_temporal_limit_data_plot <- check_temporal_limit_inspector_data[[2]]
          # Add missing date
          check_temporal_limit_data_plot <- as.data.frame(check_temporal_limit_data_plot) %>%
            dplyr::group_by(trip_id) %>%
            tidyr::complete(activity_date = seq.Date(min(trip_startdate[1],trip_enddate[1]), max(trip_startdate[1],trip_enddate[1]), by = "day"), trip_startdate = trip_startdate[1], trip_enddate = trip_enddate[1])
          # Replaces NA for missing dates
          check_temporal_limit_data_plot <- check_temporal_limit_data_plot %>% tidyr::replace_na(list(inter_activity_date = TRUE, exter_activity_date = FALSE, count_freq = 0, logical = FALSE))
          check_temporal_limit_data_plot <- subset(check_temporal_limit_data_plot, select = -c(trip_enddate))
          # Add button and data for plot in table
          check_temporal_limit <- data_button_plot(data_plot = check_temporal_limit_data_plot, data_display = check_temporal_limit, data_id = trip_select(), colname_id = "trip_id", colname_plot = c("activity_date", "logical", "count_freq"), colname_info = c("trip_id","vessel_code","trip_startdate","trip_enddate"), name_button = "button_temporal_limit")
          # Uses a function to format the table
          check_temporal_limit <- table_display_trip(check_temporal_limit, trip_select(), type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_temporal_limit <- dplyr::rename(
            .data = check_temporal_limit,
            `Details problem` = button
          )
          # Uses a function to format the table
          check_harbour <- table_display_trip(check_harbour_inspector_data, trip_select(), type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_harbour <- dplyr::rename(
            .data = check_harbour,
            `Harbour landing` = harbour_name_landing,
            `Harbour departure` = harbour_name_departure
          )
          # Uses a function to format the table
          check_raising_factor <- table_display_trip(check_raising_factor_inspector_data, trip_select(), type_inconsistency = "info")
          check_raising_factor$RF1 <- trunc(check_raising_factor$RF1 * 1000) / 1000
          # Uses a function to format the table
          check_fishing_context <- table_display_trip(check_fishing_context_inspector_data, activity_select, type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_fishing_context <- dplyr::rename(
            .data = check_fishing_context,
            `School type` = schooltype_code,
            `Number of associations object` = association_object_count
          )
          # Uses a function to format the table
          check_operationt <- table_display_trip(check_operationt_inspector_data, activity_select, type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_operationt <- dplyr::rename(
            .data = check_operationt,
            `Vessel activity` = vesselactivity_code,
            `School type` = schooltype_code,
            `Success status` = successstatus_code,
            `Weigth` = activity_weight
          )
          # Add button and data for plot in table
          check_position <- data_button_plot(data_plot = check_position_inspector_data[[2]], data_display = check_position_inspector_data[[1]], data_id = activity_select, colname_id = "activity_id", colname_plot = c("activity_position", "activity_crs"), colname_info = c("activity_id","vessel_code","trip_enddate", "activity_date", "activity_number", "type", "ocean_name", "ocean_name_pos"), name_button = "button_position")
          # Uses a function to format the table
          check_position <- table_display_trip(check_position, activity_select, type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_position <- dplyr::rename(
            .data = check_position,
            `Type` = type,
            `Ocean trip` = ocean_name,
            `Ocean activity` = ocean_name_pos,
            `Details problem` = button
          )
          # Uses a function to format the table
          check_weight <- table_display_trip(check_weight_inspector_data, activity_select, type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_weight <- dplyr::rename(
            .data = check_weight,
            `Activity weight` = activity_weight,
            `Sum catch weight` = sum_catch_weight
          )
          # Uses a function to format the table
          check_length_class <- table_display_trip(check_length_class_inspector_data, samplespeciesmeasure_select, type_inconsistency = "error")
          # Uses a function to format the table
          check_measure <- table_display_trip(check_measure_inspector_data, sample_select, type_inconsistency = "error")
          # Modify the table for display purposes: rename column
          check_measure <- dplyr::rename(
            .data = check_measure,
            `Number individuals measured sample` = sum_measuredcount,
            `Sum numbers individuals size class` = sum_count
          )
          # Uses a function to format the table
          check_temperature <- table_display_trip(check_temperature_inspector_data, activity_select, type_inconsistency = "error")
          check_temperature <- dplyr::rename(
            .data = check_temperature,
            `Sea surface temperature` = activity_seasurfacetemperature
          )
          # Uses a function to format the table
          check_species <- table_display_trip(check_species_inspector_data, samplespecies_select, type_inconsistency = "error")
          # Uses a function which indicates whether the sample is consistent with the presence of measurement
          check_sample_without_measure_inspector_data<-check_sample_without_measure_inspector(dataframe1=data_samplespecies_samplespeciesmeasure, output="report")
          # Uses a function to format the table
          check_sample_without_measure <- table_display_trip(check_sample_without_measure_inspector_data, samplespecies_select, type_inconsistency = "error")
          # Uses a function which indicates whether the sample is consistent with the presence of species
          check_sample_without_species_inspector_data<-check_sample_without_species_inspector(dataframe1=data_sample_samplespecies, output="report")
          # Uses a function to format the table
          check_sample_without_species <- table_display_trip(check_sample_without_species_inspector_data, sample_select, type_inconsistency = "error")
          # Checks data consistency
          if(nrow(data_sample)!=length(sample_select$sample_id)){warning(text_object_more_or_less(id=sample_select$sample_id ,result_check=data_sample$sample_id))}
          if(nrow(data_samplespecies)!=length(samplespecies_select$samplespecies_id)){warning(text_object_more_or_less(id=samplespecies_select$samplespecies_id, result_check=data_sample$samplespecies_id))}
          # Uses a function which indicates whether the sample is consistent with the subsample number
          check_super_sample_number_consistent_inspector_data<-check_super_sample_number_consistent_inspector(dataframe1=data_sample, dataframe2=data_samplespecies, output="report")
          # Uses a function to format the table
          check_super_sample_number_consistent <- table_display_trip(check_super_sample_number_consistent_inspector_data, sample_select, type_inconsistency = "error")
          check_super_sample_number_consistent <- dplyr::rename(
            .data = check_super_sample_number_consistent,
            `Super sample` = sample_supersample,
            `Counts number sub-sample numbers not 0` = count_subsamplenumber_N0,
            `Counts number sub-sample numbers equal 0` = count_subsamplenumber_0,
            `Counts number sub-sample numbers equal 1` = count_subsamplenumber_1,
            `Counts number sample species` = count_samplespecies
          )
          # Uses a function which indicates whether the sample well number is consistent with the associated trip well numbers
          check_well_number_consistent_inspector_data<-check_well_number_consistent_inspector(dataframe1=data_sample, dataframe2=data_well, output="report")
          # Uses a function to format the table
          check_well_number_consistent <- table_display_trip(check_well_number_consistent_inspector_data, sample_select, type_inconsistency = "error")
          check_well_number_consistent <- dplyr::rename(
            .data = check_well_number_consistent,
            `Well` = sample_well
          )
          # Checks data consistency
          if(nrow(data_samplespeciesmeasure)!=length(samplespeciesmeasure_select$samplespeciesmeasure_id)){warning(text_object_more_or_less(id=samplespeciesmeasure_select$samplespeciesmeasure_id ,result_check=data_samplespeciesmeasure$samplespeciesmeasure_id))}
          # Uses a function which indicates whether the sample is consistent for the percentage of little and big fish sampled
          check_little_big_inspector_data<-check_little_big_inspector(dataframe1=data_sample, dataframe2=data_samplespecies, dataframe3 = data_samplespeciesmeasure , output="report")
          # Uses a function to format the table
          check_little_big <- table_display_trip(check_little_big_inspector_data, sample_select, type_inconsistency = "error")
          check_little_big$little_percentage <- trunc(check_little_big$little_percentage * 1000) / 1000
          check_little_big$big_percentage <- trunc(check_little_big$big_percentage * 1000) / 1000
          check_little_big$measure1_percentage <- trunc(check_little_big$measure1_percentage * 1000) / 1000
          check_little_big$measure2_percentage <- trunc(check_little_big$measure2_percentage * 1000) / 1000
          check_little_big <- dplyr::rename(
            .data = check_little_big,
            `Smalls weight` = sample_smallsweight,
            `Bigs weight` = sample_bigsweight,
            `Total weight` = sample_totalweight,
            `Little %` = little_percentage,
            `Big %` = big_percentage,
            `Measurement type FL %` = measure1_percentage,
            `Measurement type PD1 %` = measure2_percentage
          )
          # Checks data consistency
          if(nrow(data_trip)!=length(trip_select()$trip_id)){warning(text_object_more_or_less(id=trip_select()$trip_id ,result_check=data_trip$trip_id))}
          # Uses a function which indicates whether the sample is consistent for the weighting
          check_weighting_inspector_data<-check_weighting_inspector(dataframe1=data_sample, dataframe2=data_sampleactivity, dataframe3 = data_trip, dataframe4 = data_landing , output="report")
          # Uses a function to format the table
          check_weighting <- table_display_trip(check_weighting_inspector_data, sample_select, type_inconsistency = "error")
          check_weighting <- dplyr::rename(
            .data = check_weighting,
            `Smalls weight` = sample_smallsweight,
            `Bigs weight` = sample_bigsweight,
            `Total weight` = sample_totalweight,
            `Sample type` = sampletype_code,
            `Sum weighted weights` = weightedweight,
            `Vessel type` = vesseltype_label1,
            `Sum weight fresh landings baitboats` = sum_landing_weight
            )
          # Uses a function which indicates whether the sample weight (m10 and p10) is consistent for the global weight
          check_weight_sample_inspector_data<-check_weight_sample_inspector(dataframe1=data_sample, output="report")
          # Uses a function to format the table
          check_weight_sample <- table_display_trip(check_weight_sample_inspector_data, sample_select, type_inconsistency = "error")
          check_weight_sample <- dplyr::rename(
            .data = check_weight_sample,
            `Smalls weight` = sample_smallsweight,
            `Bigs weight` = sample_bigsweight,
            `Total weight` = sample_totalweight
          )
          # Uses a function which indicates whether the sample and the existence of the activity 
          check_activity_sample_inspector_data<-check_activity_sample_inspector(dataframe1=data_sample_activity, output="report")
          # Uses a function to format the table
          check_activity_sample <- table_display_trip(check_activity_sample_inspector_data, sample_select, type_inconsistency = "error")
          check_activity_sample <- dplyr::rename(
            .data = check_activity_sample,
            `Counts activity` = count_activity
          )
          # Uses a function which indicates whether the sample measurement types is consistent for the species or weight values
          check_ldlf_inspector_data<-check_ldlf_inspector(dataframe1=data_samplespecies, dataframe2 = data_sample, output="report")
          # Uses a function to format the table
          check_ldlf <- table_display_trip(check_ldlf_inspector_data, samplespecies_select, type_inconsistency = "error")
          check_ldlf <- dplyr::rename(
            .data = check_ldlf,
            `Smalls weight` = sample_smallsweight,
            `Bigs weight` = sample_bigsweight,
            `Total weight` = sample_totalweight
          )
          # Uses a function which indicates whether the small and large sample weights is consistent for the sum of the small and big weights of the associated well
          check_distribution_inspector_data<-check_distribution_inspector(dataframe1=data_sample,dataframe2 = data_well, dataframe3 = data_wellactivity, dataframe4 = data_wellactivityspecies, output="report")
          # Uses a function to format the table
          check_distribution <- table_display_trip(check_distribution_inspector_data, sample_select, type_inconsistency = "error")
          check_distribution <- dplyr::rename(
            .data = check_distribution,
            `Smalls weight` = sample_smallsweight,
            `Bigs weight` = sample_bigsweight,
            `Well` = sample_well,
            `Smalls weight well` = weight_small_total,
            `Bigs weight well` = weight_big
          )
          return(list(check_trip_activity, check_fishing_time, check_sea_time, check_landing_consistent, check_landing_total_weigh, check_temporal_limit, check_harbour, check_raising_factor, check_fishing_context, check_operationt,check_position,check_weight,check_length_class,check_measure, check_temperature, check_species, check_sample_without_measure, check_sample_without_species, check_super_sample_number_consistent, check_well_number_consistent, check_little_big, check_weighting, check_weight_sample, check_activity_sample, check_ldlf, check_distribution))
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
      if (text_error_trip_select() != TRUE) {
        showNotification(id = "notif_warning", ui = text_error_trip_select(), type = "error")
        return(paste0("<span style=\"color:red\">", text_error_trip_select(), "</span>"))
      }
      # If the connection file is missing
      if (text_error_trip_select() == TRUE && !isTruthy(config_data())) {
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
        text <- "Finished calculation"
        showNotification(id = "notif_default", ui = text, type = "default")
        return(paste0("<span style=\"color:#34C909\">", text, "</span>"))
      }
    })
  })
}

# Shiny function : table display
table_server <- function(id, data, number, parent_in, text_error_trip_select, trip_select, calcul_check, autoWidth = FALSE, columnDefs = NULL) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT(
      {
        # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
        if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
          data <- data()[[number]]
          if (parent_in$type_line_check_trip == "inconsistent") {
            return(data[data$Check != as.character(icon("check")), ])
          } else {
            return(data)
          }
        }
      },
      escape = FALSE, rownames = FALSE,
      extensions = "Buttons", 
      options = list(lengthChange = FALSE, scrollX = TRUE, autoWidth= autoWidth, columnDefs = unlist(list(list(list(className = 'dt-left', targets = "_all")),columnDefs ),recursive = F),  dom = 'Bfrtip',
                     buttons = 
                       list(list(extend = "copy", text ="Copy data display"),
                       list(extend = "collection", text = "Download data all", action = DT::JS(paste0("function ( e, dt, node, config ) {Shiny.setInputValue('button_download', ",number,", {priority: 'event'});}"))))))
  })
}

window_button_download <- function(number) {
      modalDialog(downloadButton(outputId="download_csv",label="CSV"),
                  downloadButton(outputId="download_excel",label="Excel"),
                  fade = TRUE,
                  easyClose = TRUE,
                  footer = NULL, 
                  title = "Download Table")
}

# Shiny function : table display
table_ui <- function(id, title, text = NULL) {
  div(
    id = paste0("div_", id),
    class = "col-sm-12 col-md-6 col-lg-4",
    box(
      width = "100%",
      title = title,
      status = "primary", 
      solidHeader = TRUE,
      HTML(text),
      withSpinner(DTOutput(NS(namespace = id, id = "table")), type = 6, size = 0.5, proxy.height = "70px")
    )
  )
}

# Function which formats the trip data for display inconsistency
table_display_trip <- function(data, data_info, type_inconsistency) {
  # Retrieves the name of the column containing the ID
  colname_id <- grep("_id$", colnames(data), value = TRUE)
  # Deletes duplicate columns
  all_colname<-c(colnames(data), colnames(data_info))
  colname_double<-table(all_colname)[table(all_colname)>1]
  colname_double<-names(colname_double)[!(names(colname_double) %in% colname_id)]
  data<-data[,!(colnames(data) %in% colname_double)]
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
      `Activity number` = activity_number
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
      `FAO code` = specie_name,
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

# Function to create the button in the table that will create the plot
data_button_plot <- function(data_plot, data_display, data_id, colname_id, colname_plot, colname_info,name_button){
  # Add line identification 
  data_plot <- merge(data_id, data_plot, by.x = colname_id, by.y = colname_id)
  # Add button and data for plot in table
  code_txt<-paste0('data_plot <- data_plot %>%dplyr::group_by(',colname_id,') %>%
  dplyr::reframe(buttontmp = paste0("button&", paste0(deparse(dplyr::across(.cols=c(',paste0(colname_plot,collapse= ','),'))), collapse = ""), "&",',paste0(colname_info,collapse= ', "&",'),'))%>%
  dplyr::group_by(',colname_id,') %>% dplyr::filter(dplyr::row_number() == 1)')
  eval(parse(text=code_txt))
  data_display <- merge(data_display, data_plot, by = colname_id)
  data_display$button <- NA
   data_display$button[data_display$logical == FALSE] <- sapply(which(data_display$logical == FALSE), function(c) {
     as.character(shiny::actionButton(inputId = data_display$buttontmp[c], label = "Detail", onclick = paste0('Shiny.setInputValue(\"',name_button,'\", this.id, {priority: \"event\"})')))
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
      ),
      legend = list(orientation = 'h', y=-0.3)
    )
}

# Function to create the plot of the consistency of the position for the activity
plot_position<- function(data){
  # Spatial formatting
  data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = unique(data$activity_crs)) %>% dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
  # Plot
  plotly::plot_ly(
    data = data_geo,  lat = ~Y, lon = ~X, type = "scattermapbox", mode = "markers",  marker = list(size = 10), hovertemplate = "(%{lat}°,%{lon}°)<extra></extra>")%>%
    plotly::layout(showlegend=FALSE,mapbox = list(style = "carto-positron", center = list(lon = data_geo$X, lat = data_geo$Y)))
  
}

 # Function to list elements with a number of occurrences other than 2
text_object_more_or_less<-function(id,result_check){
  all<-c(id,result_check)
  number_occurrences<-table(all)
  text<-""
  if(sum(number_occurrences==1)>0){
    text<-paste0(text,"Missing item ","(",sum(number_occurrences==1),"):",paste0(names(number_occurrences[number_occurrences==1]), collapse = ", "),"\n")
  }
  if(sum(number_occurrences>2)>0){
    text<-paste0(text,"Too many item ","(",sum(number_occurrences>2),"):",paste0(names(number_occurrences[number_occurrences>2]), collapse = ", "))
  }
  return(text)
}



