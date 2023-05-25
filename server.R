

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
    if (is.null(input$setting_file_path$datapath) && file.exists(file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"))) {
      path_setting_file <- file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")
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
          readLines(file.path(".", "sql", "trip_id.sql")),
          collapse = "\n"
        )
        # Transform the SQL query
        # Deletes the part linked to the selection with a date range
        if (isTruthy(input$vessel_number) && isTruthy(input$trip_end_date)) {
          trip_id_sql <- sub(
            pattern = "(t.startdate >= ?select_item_2 AND t.enddate <= ?select_item_3)",
            replacement = "",
            x = trip_id_sql,
            fixed = TRUE
          )
          select_item_2 <- input$vessel_number
          select_item_3 <- input$trip_end_date
        }
        # Deletes the part linked to a selection with the vessel code and the end date of the trip
        if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range)) {
          trip_id_sql <- sub(
            pattern = "(v.code IN (?select_item_2) AND t.enddate IN (?select_item_3))",
            replacement = "",
            x = trip_id_sql,
            fixed = TRUE
          )
          select_item_2 <- input$trip_start_date_range
          select_item_3 <- input$trip_end_date_range
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
          readLines(file.path(".", "sql", "trip_enddate_vessel_code.sql")),
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
        # Uses a function which indicates whether the selected trips contain the trip sharbour of departure of the current trip inconsistent with the harbour of landing of the previous trip
        check_harbour_inspector_data <- check_harbour_inspector(
          data_connection = data_connection,
          type_select = "trip",
          select = trip_select()$trip_id,
          output = "report",
          logbook_program = config_data()[["logbook_program"]]
        )
        # Disconnection to the base
        DBI::dbDisconnect(data_connection[[2]])
        trip_enddate_vessel_code_data$trip_enddate <- as.character(trip_enddate_vessel_code_data$trip_enddate)
        # Uses a function to format the table
        check_trip_activity <- table_display_trip(check_trip_activity_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "warning")
        # Modify the table for display purposes: rename column
        check_trip_activity <- dplyr::rename(
          .data = check_trip_activity,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical
        )
        # Uses a function to format the table
        check_fishing_time <- table_display_trip(check_fishing_time_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "error")
        # Modify the table for display purposes: rename column
        check_fishing_time <- dplyr::rename(
          .data = check_fishing_time,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Trip fishing time` = trip_fishing_time,
          `Sum route fishing time` = sum_route_fishingtime
        )
        # Uses a function to format the table
        check_sea_time <- table_display_trip(check_sea_time_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "error")
        # Modify the table for display purposes: rename column
        check_sea_time <- dplyr::rename(
          .data = check_sea_time,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Trip sea time` = trip_sea_time,
          `Sum route sea time` = sum_route_seatime
        )
        # Uses a function to format the table
        check_landing_consistent <- table_display_trip(check_landing_consistent_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "warning")
        # Modify the table for display purposes: rename column
        check_landing_consistent <- dplyr::rename(
          .data = check_landing_consistent,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Vessel capacity` = vessel_capacity,
          `Total weight` = trip_weighttotal
        )
        # Uses a function to format the table
        check_landing_total_weigh <- table_display_trip(check_landing_total_weight_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "error")
        # Modify the table for display purposes: rename column
        check_landing_total_weigh <- dplyr::rename(
          .data = check_landing_total_weigh,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Trip landing weight` = trip_landingtotalweight,
          `Sum landing weight` = sum_weightlanding
        )
        # Data preparation
        check_temporal_limit <- check_temporal_limit_inspector_data[[1]]
        check_temporal_limit_data_plot <- check_temporal_limit_inspector_data[[2]]
        # Add missing date
        check_temporal_limit_data_plot <- as.data.frame(check_temporal_limit_data_plot) %>%
          group_by(trip_id) %>%
          tidyr::complete(activity_date = seq.Date(trip_startdate[1], trip_enddate[1], by = "day"), trip_startdate = trip_startdate[1], trip_enddate = trip_enddate[1])
        # Replaces NA for missing dates
        check_temporal_limit_data_plot <- check_temporal_limit_data_plot %>% tidyr::replace_na(list(inter_activity_date = TRUE, exter_activity_date = FALSE, count_freq = 0, logical = FALSE))
        # Add vessel code
        check_temporal_limit_data_plot <- subset(check_temporal_limit_data_plot, select = -c(trip_enddate))
        check_temporal_limit_data_plot <- merge(trip_enddate_vessel_code_data, check_temporal_limit_data_plot, by.x = "trip_id", by.y = "trip_id")
        # Add button and data for plot in table
        check_temporal_limit_data_plot <- check_temporal_limit_data_plot %>%
          dplyr::group_by(trip_id) %>%
          dplyr::summarise(buttontmp = paste0("button&", paste0(deparse(dplyr::across()), collapse = ""), "&", trip_id, "&", vessel_code), .groups = "keep") %>%
          dplyr::filter(dplyr::row_number() == 1)
        check_temporal_limit <- merge(check_temporal_limit, check_temporal_limit_data_plot, by = "trip_id")
        check_temporal_limit$button <- NA
        check_temporal_limit$button[check_temporal_limit$logical == FALSE] <- sapply(which(check_temporal_limit$logical == FALSE), function(c) {
          as.character(shiny::actionButton(inputId = check_temporal_limit$buttontmp[c], label = "Detail", onclick = 'Shiny.setInputValue(\"button\", this.id, {priority: \"event\"})'))
        })
        check_temporal_limit <- subset(check_temporal_limit, select = -c(buttontmp))
        # Uses a function to format the table
        check_temporal_limit <- table_display_trip(check_temporal_limit, trip_enddate_vessel_code_data, type_inconsistency = "error")
        # Modify the table for display purposes: rename column
        check_temporal_limit <- dplyr::rename(
          .data = check_temporal_limit,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Details problem` = button
        )
        # Uses a function to format the table
        check_harbour <- table_display_trip(check_harbour_inspector_data, trip_enddate_vessel_code_data, type_inconsistency = "error")
        # Modify the table for display purposes: rename column
        check_harbour <- dplyr::rename(
          .data = check_harbour,
          `Vessel code` = vessel_code,
          `Trip enddate` = trip_enddate,
          Check = logical,
          `Harbour landing` = harbour_name_landing,
          `Harbour departure` = harbour_name_departure
        )
        return(list(check_trip_activity, check_fishing_time, check_sea_time, check_landing_consistent, check_landing_total_weigh, check_temporal_limit, check_harbour))
      }
    }
  })
  
  # Table of consistency test of the presence of activities
  output$check_trip_activity <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[1]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
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
        data <- calcul_check()[[2]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Table of consistency test of the sea time
  output$check_sea_time <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[3]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Table of consistency test of the landing total weight with vessel capacity
  output$check_landing_consistent <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[4]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Table of consistency test of the total landed weight for canneries is consistent with the weights of each landing for the canneries
  output$check_landing_total_weigh <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[5]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Table of consistency test of trip start and end date is consistent with the the dates of activity
  output$check_temporal_limit <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[6]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  output$plot <- renderPlotly({
    splitID <- strsplit(input$button, "&")[[1]]
    data <- eval(parse(text = splitID[[2]]))
    plot_temporal_limit(data)
  })
  
  observeEvent(input$button, {
    splitID <- strsplit(input$button, "&")[[1]]
    data <- eval(parse(text = splitID[[2]]))
    vessel_code <- splitID[4]
    showModal(modalDialog(
      plotlyOutput("plot"),
      title = paste0("Vessel code : ", vessel_code, ", trip end date : ", data$trip_enddate[1]),
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  # Table of consistency test of the harbour of landing of the previous trip and the harbour of departure of the current trip
  output$check_harbour <- renderDT(
    {
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (text_error_trip_select() == TRUE && is.data.frame(trip_select()) && isTruthy(calcul_check())) {
        data <- calcul_check()[[7]]
        if (input$type_line_check_trip == "inconsistent") {
          return(data[data$Check != as.character(icon("check")), ])
        } else {
          return(data)
        }
      }
    },
    escape = FALSE,
    options = list(lengthChange = FALSE, scrollX = TRUE),
    rownames = FALSE
  )
  
  # Management of the display or not of the boxes in the trip tab
  observeEvent(input$type_check_trip, {
    if (input$type_check_trip == "All") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_fishing_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_landing_consistent", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_total_weigh", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_temporal_limit", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check")), where = "afterEnd")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_harbour", anim = TRUE, animType = "fade")
    }
    if (input$type_check_trip == "Warning") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_fishing_time", anim = FALSE)
      shinyjs::hide(id = "div_check_sea_time", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_total_weigh", anim = FALSE)
      shinyjs::hide(id = "div_check_temporal_limit", anim = FALSE)
      shinyjs::hide(id = "div_check_harbour", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, time = 1, animType = "fade")
    }
    if (input$type_check_trip == "Error") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_trip_activity", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_consistent", anim = FALSE)
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_total_weigh", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_landing_total_weigh", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_temporal_limit", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check")), where = "afterEnd")
      shinyjs::show(id = "div_check_harbour", anim = TRUE, animType = "fade")
    }
  })
  
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
