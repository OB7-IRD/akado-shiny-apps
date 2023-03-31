

shinyServer(function(input, output, session) {
  # Error message if the date range is not correct
  output$error_date_select <- renderText({
    if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range) && input$trip_start_date_range > input$trip_end_date_range) {
      "Error: start date must be before end date"
    }
  })
  
  # Error message if the trip selection elements are not correctly filled in
  text_error_trip_select <- eventReactive(input$start_button, {
    # if no selection element is filled in
    if (sum(isTruthy(input$vessel_number), isTruthy(input$trip_end_date)) == 0 && sum(isTruthy(input$trip_start_date_range), isTruthy(input$trip_end_date_range)) == 0) {
      return("Error: please select a trip")
    }
    # if there are elements filled in for several types of selection
    if (sum(isTruthy(input$vessel_number), isTruthy(input$trip_end_date)) > 0 && sum(isTruthy(input$trip_start_date_range), isTruthy(input$trip_end_date_range)) > 0) {
      return("Error: please choose only one type of trip selection")
    }
    # if there are missing selection elements for a selection type
    if (sum(isTruthy(input$vessel_number), isTruthy(input$trip_end_date)) == 1 || sum(isTruthy(input$trip_start_date_range), isTruthy(input$trip_end_date_range)) == 1) {
      return("Error: Please fill in the missing selection item")
    }
    # if the end date is earlier than the start date
    if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range) && input$trip_start_date_range > input$trip_end_date_range) {
      return("Error: start date must be before end date")
    }
    return(TRUE)
  })
  
  # Read the .yml file of configuration for the connection
  config_data <- eventReactive(input$start_button, {
    # If the user has not specified a file and the file exists in the default path, indicates the default path
    if (is.null(input$setting_file_path$datapath) && file.exists(paste0(path.expand("~"), "/.appconfig/configuration_file.yml"))) {
      path_setting_file <- paste0(path.expand("~"), "/.appconfig/configuration_file.yml")
      # If the user has specified a file, indicates the path to specify
    } else if (!is.null(input$setting_file_path$datapath)) {
      path_setting_file <- input$setting_file_path$datapath
    }
    # Read the file if the path is available
    if (exists("path_setting_file")) {
      furdeb::configuration_file(
        path_file = path_setting_file,
        silent = TRUE
      )
    }
  })
  
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
      # If the database is "observe_9a", read, transform and execute the SQL query that selects the trips according to the user parameters
      if (data_connection[1] == "observe_9a") {
        # Read the SQL query
        trip_id_sql <- paste(
          readLines("./sql/trip_id.sql"),
          collapse = "\n"
        )
        # Transform the SQL query
        # Deletes the part linked to the selection with a date range
        if (isTruthy(input$vessel_number) && isTruthy(input$trip_end_date)) {
          trip_id_sql <- sub(
            pattern = "(t.startdate >= ?select_item_1 AND t.enddate <= ?select_item_2)",
            replacement = "",
            x = trip_id_sql,
            fixed = TRUE
          )
          select_item_1 <- input$vessel_number
          select_item_2 <- input$trip_end_date
        }
        # Deletes the part linked to a selection with the vessel code and the end date of the trip
        if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range)) {
          trip_id_sql <- sub(
            pattern = "(v.code IN (?select_item_1) AND t.enddate IN (?select_item_2))",
            replacement = "",
            x = trip_id_sql,
            fixed = TRUE
          )
          select_item_1 <- input$trip_start_date_range
          select_item_2 <- input$trip_end_date_range
        }
        # Replaces the anchors with the selected values
        trip_id_sql <- DBI::sqlInterpolate(
          conn = data_connection[[2]],
          sql = trip_id_sql,
          select_item_1 = DBI::SQL(paste0("'", select_item_1, "'")),
          select_item_2 = DBI::SQL(paste0("'", select_item_2, "'"))
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
  
  # Performs all calculations to test for inconsistencies
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
      # If the database is "observe_9a", read, transform and execute the SQL query that selects the trips according to the user parameters
      if (data_connection[1] == "observe_9a") {
        # Read the SQL query to retrieve the vessel code and the end of the trip of all the trips that have been selected
        trip_enddate_vessel_code_sql <- paste(
          readLines("./sql/trip_enddate_vessel_code.sql"),
          collapse = "\n"
        )
        # Replaces the anchors with the selected values
        trip_enddate_vessel_code_sql <- DBI::sqlInterpolate(
          conn = data_connection[[2]],
          sql = trip_enddate_vessel_code_sql,
          select_item = DBI::SQL(paste(paste0("'", trip_select()$trip_id, "'"),
                                       collapse = ", "
          ))
        )
        # Execute the SQL query
        trip_enddate_vessel_code_data <- dplyr::tibble(DBI::dbGetQuery(
          conn = data_connection[[2]],
          statement = trip_enddate_vessel_code_sql
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
        # Disconnection to the base
        DBI::dbDisconnect(data_connection[[2]])
        trip_enddate_vessel_code_data$trip_enddate <- as.character(trip_enddate_vessel_code_data$trip_enddate)
        # Uses a function to format the table
        check_trip_activity<-table_display_trip(check_trip_activity_inspector_data,trip_enddate_vessel_code_data)
        # Modify the table for display purposes: rename column
        check_trip_activity <- dplyr::rename(
          .data = check_trip_activity,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Activity = logical
        )
        # Uses a function to format the table
        check_fishing_time<-table_display_trip(check_fishing_time_inspector_data,trip_enddate_vessel_code_data)
        # Modify the table for display purposes: rename column
        check_fishing_time <- dplyr::rename(
          .data = check_fishing_time,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Activity = logical,
          `Trip fishing time` = trip_fishing_time,
          `Sum route fishing time` = sum_route_fishingtime
        )
         return(list(check_trip_activity, check_fishing_time))
      }
    }
  })
  
  # Table of consistency test of the presence of activities
  output$check_trip_activity <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        return(calcul_check()[[1]])
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Table of consistency test of the fishing time
  output$check_fishing_time <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        return(calcul_check()[[2]])
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Displays the errors and notifications that occur when you want to start the calculations
  output$error_trip_select <- renderText({
    # If there are errors in the selection parameters
    if (text_error_trip_select() != TRUE) {
      showNotification(id = "notif_warning", ui = text_error_trip_select(), type = "error")
      return(paste0("<span style=\"color:red\">", text_error_trip_select(), "</span>"))
    }
    # If the connection file is missing
    if (text_error_trip_select() == TRUE && !isTruthy(config_data())) {
      text <- "Error: There is no configuration file for the connection to the base"
      showNotification(id = "notif_warning", ui = text, type = "error")
      return(paste0("<span style=\"color:red\">", text, ", please either select one using the \"settings\" tab or put it in ", path.expand("~"), "/.appconfig/configuration_file.yml)</span>"))
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
