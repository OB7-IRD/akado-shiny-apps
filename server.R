

shinyServer(function(input, output, session) {
  # Error message if the date range is not correct
  output$error_date_select <- renderText({
    if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range) && input$trip_start_date_range > input$trip_end_date_range) {
      "Error: start date must be before end date"
    }
  })
  
  # Error message if the trip selection elements are not correctly filled in
  text_error_trip_select <- text_error_trip_select_server(id = "start_button", parent_in = input)
  
  # Read the .yml file of configuration for the connection
  config_data <- config_data_server(id = "start_button", parent_in = input)
  
  # Retrieves the list of trips selected by the user
  trip_select <- trip_select_server(id = "start_button", parent_in = input, text_error_trip_select = text_error_trip_select, config_data = config_data)
  
  # Performs all calculations to test for inconsistencies
  calcul_check <- calcul_check_server(id = "start_button", text_error_trip_select = text_error_trip_select, trip_select = trip_select, config_data = config_data)
  
  # Displays the errors and notifications that occur when you want to start the calculation
  error_trip_select_serveur(id = "error_trip_select", text_error_trip_select = text_error_trip_select, config_data = config_data, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the presence of activities
  table_server(id = "check_trip_activity", data = calcul_check, number = 1, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the fishing time
  table_server(id = "check_fishing_time", data = calcul_check, number = 2, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the sea time
  table_server(id = "check_sea_time", data = calcul_check, number = 3, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the landing total weight with vessel capacity
  table_server(id = "check_landing_consistent", data = calcul_check, number = 4, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the total landed weight for canneries is consistent with the weights of each landing for the canneries
  table_server(id = "check_landing_total_weigh", data = calcul_check, number = 5, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of trip start and end date is consistent with the the dates of activity
  table_server(id = "check_temporal_limit", data = calcul_check, number = 6, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Date control plot, display in a window
  output$plot_temporal_limit <- renderPlotly({
    splitID <- strsplit(input$button_temporal_limit, "&")[[1]]
    data <- eval(parse(text = splitID[[2]]))
    startdate <- as.Date(x=splitID[5], format="%Y-%m-%d")
    enddate <- as.Date(x=splitID[6], format="%Y-%m-%d")
    plot_temporal_limit(data,startdate,enddate)
  })
  
  # Date control window
  observeEvent(input$button_temporal_limit, {
    splitID <- strsplit(input$button_temporal_limit, "&")[[1]]
    vessel_code <- splitID[4]
    enddate <- splitID[6]
    # Non-breaking hyphen (-)
    enddate<- gsub("-","&#8209;",enddate)
    showModal(modalDialog(
      fluidRow(column(3,
                      style = "padding-left:5px;padding-right:0px;",
                      HTML(paste0("<b>Trip information : </b><br> 
                             <ul><li>Vessel code : ", vessel_code, "</li>
                             <li>Trip end date : ", enddate, "</li></ul>"))),
      column(9,
             style = "padding-left:0px;padding-right:5px;",
             plotlyOutput("plot_temporal_limit"))),
      title = "Time coverage detail",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Table of consistency test of the harbour of landing of the previous trip and the harbour of departure of the current trip
  table_server(id = "check_harbour", data = calcul_check, number = 7, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test of the harbour of RF1 and limit values
  table_server(id = "check_raising_factor", data = calcul_check, number = 8, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test school type and association
  table_server(id = "check_fishing_context", data = calcul_check, number = 9, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px"),list(targets = c(2), width = "73px")))
  
  # Table of consistency test the succes status and the vessel activity, the type of school or the weight caught
  table_server(id = "check_operationt", data = calcul_check, number = 10, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px"),list(targets = c(2), width = "73px")))
  
  # Table of consistency test the ocean declaration and activity position
  table_server(id = "check_position", data = calcul_check, number = 11, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px"),list(targets = c(2), width = "73px")))
  
  # Position control plot, display in a window
  output$plot_position <- renderPlotly({
    splitID <- strsplit(input$button_position, "&")[[1]]
    data <- eval(parse(text = splitID[[2]]))
    plot_position(data)
  })
  
  # Position control window
  observeEvent(input$button_position, {
    splitID <- strsplit(input$button_position, "&")[[1]]
    enddate <- splitID[5]
    activity_date <- splitID[5]
    # Spatial formatting
    data <- eval(parse(text = splitID[[2]]))
    data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = "activity_crs") %>% dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.)))
    # Non-breaking hyphen (-)
    enddate<- gsub("-","&#8209;",enddate)
    activity_date<- gsub("-","&#8209;",activity_date)
    showModal(modalDialog(
      fluidRow(column(3,
                      style = "padding-left:5px;padding-right:0px;",
                      HTML(paste0("<b>Trip information : </b><br> 
                             <ul><li>Vessel code : ", splitID[4], "</li>
                             <li>Trip end date : ", enddate, "</li>
                             <li>Activity date : ", activity_date, "</li>
                             <li>Activity number : ", splitID[7], "</li>
                             <li>Latitude : ", data_geo$Y, "°</li>
                             <li>Longitude : ", data_geo$X, "°</li></ul>
                             <b>Problem information : </b><br>
                             <ul><li>Type : ", splitID[8], "</li>
                             <li>Ocean trip : ", splitID[9], "</li>
                             <li>Ocean activity : ", splitID[10], "</li></ul>"))),
               column(9,
                      style = "padding-left:0px;padding-right:5px;",
                      plotlyOutput("plot_position"))),
      title = "Position",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Table of consistency test the sum of the weight indicated for the catch and activity weight
  table_server(id = "check_weight", data = calcul_check, number = 12, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px"),list(targets = c(2), width = "73px")))
  
  # Table of consistency test the size class of the samples depending on the species and measurement type is consistent with valid limits
  table_server(id = "check_length_class", data = calcul_check, number = 13, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Table of consistency test the total number of individuals measured per sample is consistent with the sum of individuals per sample, species and size class
  table_server(id = "check_measure", data = calcul_check, number = 14, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Table of consistency test the sea surface temperature is consistent with valid limits
  table_server(id = "check_temperature", data = calcul_check, number = 15, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px"),list(targets = c(2), width = "73px")))
  
  # Table of consistency test the species sampled is consistent with species authorized
  table_server(id = "check_species", data = calcul_check, number = 16, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Table of consistency test the sample is consistent with the presence of measurement
  table_server(id = "check_sample_without_measure", data = calcul_check, number = 17, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Table of consistency test the sample is consistent with the presence of species
  table_server(id = "check_sample_without_species", data = calcul_check, number = 18, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)

  # Table of consistency test the sample is consistent with the subsample number
  table_server(id = "check_super_sample_number", data = calcul_check, number = 19, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Table of consistency test the sample well number is consistent with the associated trip well numbers
  table_server(id = "check_well_number", data = calcul_check, number = 20, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check)
  
  # Table of consistency test the sample is consistent for the percentage of little and big fish sampled 
  table_server(id = "check_little_big", data = calcul_check, number = 21, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))

  # Table of consistency test the sample is consistent for the weighting 
  table_server(id = "check_weighting", data = calcul_check, number = 22, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, autoWidth = TRUE, columnDefs = list(list(targets = c(1), width = "73px")))
  
  # Management of the display or not of the boxes in the trip tab
  observeEvent(input$type_check_trip, {
    if (input$type_check_trip == "All") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      # Trip
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_fishing_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_landing_consistent", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_total_weigh", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_temporal_limit", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_harbour", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_raising_factor", anim = TRUE, animType = "fade")
      # Activity
      shinyjs::show(id = "div_check_fishing_context", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_operationt", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_operationt", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_position", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_position", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weight", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_temperature", anim = TRUE, animType = "fade")
      # Sample
      shinyjs::show(id = "div_check_length_class", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_species", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_species", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_super_sample_number", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_well_number", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_little_big", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weighting", anim = TRUE, animType = "fade")
      }
    if (input$type_check_trip == "Info") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_fishing_time", anim = FALSE)
      shinyjs::hide(id = "div_check_sea_time", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_total_weigh", anim = FALSE)
      shinyjs::hide(id = "div_check_temporal_limit", anim = FALSE)
      shinyjs::hide(id = "div_check_harbour", anim = FALSE)
      shinyjs::hide(id = "div_check_trip_activity", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_consistent", anim = FALSE)
      shinyjs::hide(id = "div_check_fishing_context", anim = FALSE)
      shinyjs::hide(id = "div_check_operationt", anim = FALSE)
      shinyjs::hide(id = "div_check_position", anim = FALSE)
      shinyjs::hide(id = "div_check_weight", anim = FALSE)
      shinyjs::hide(id = "div_check_temperature", anim = FALSE)
      shinyjs::hide(id = "div_check_length_class", anim = FALSE)
      shinyjs::hide(id = "div_check_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_species", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_species", anim = FALSE)
      shinyjs::hide(id = "div_check_super_sample_number", anim = FALSE)
      shinyjs::hide(id = "div_check_well_number", anim = FALSE)
      shinyjs::hide(id = "div_check_little_big", anim = FALSE)
      shinyjs::hide(id = "div_check_weighting", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_raising_factor", anim = TRUE, animType = "fade")
    }
    if (input$type_check_trip == "Warning") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_fishing_time", anim = FALSE)
      shinyjs::hide(id = "div_check_sea_time", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_total_weigh", anim = FALSE)
      shinyjs::hide(id = "div_check_temporal_limit", anim = FALSE)
      shinyjs::hide(id = "div_check_harbour", anim = FALSE)
      shinyjs::hide(id = "div_check_raising_factor", anim = FALSE)
      shinyjs::hide(id = "div_check_fishing_context", anim = FALSE)
      shinyjs::hide(id = "div_check_operationt", anim = FALSE)
      shinyjs::hide(id = "div_check_position", anim = FALSE)
      shinyjs::hide(id = "div_check_weight", anim = FALSE)
      shinyjs::hide(id = "div_check_temperature", anim = FALSE)
      shinyjs::hide(id = "div_check_length_class", anim = FALSE)
      shinyjs::hide(id = "div_check_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_species", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_species", anim = FALSE)
      shinyjs::hide(id = "div_check_super_sample_number", anim = FALSE)
      shinyjs::hide(id = "div_check_well_number", anim = FALSE)
      shinyjs::hide(id = "div_check_little_big", anim = FALSE)
      shinyjs::hide(id = "div_check_weighting", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, time = 1, animType = "fade")
    }
    if (input$type_check_trip == "Error") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_trip_activity", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_consistent", anim = FALSE)
      shinyjs::hide(id = "div_check_raising_factor", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_total_weigh", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_landing_total_weigh", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_temporal_limit", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_harbour", anim = TRUE, animType = "fade")
      # Activity
      shinyjs::show(id = "div_check_fishing_context", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_operationt", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_operationt", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_position", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_position", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weight", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_temperature", anim = TRUE, animType = "fade")
      # Sample
      shinyjs::show(id = "div_check_length_class", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_species", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_species", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_super_sample_number", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_well_number", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_little_big", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weighting", anim = TRUE, animType = "fade")
    }
  })
  
  # Summary page text
  output$text_summary<- renderText({
    # Grouping of data sets with the addition of the group number, then selection of lines containing inconsistencies
    data_regroup <- calcul_check() %>% bind_rows(.id = "group_id") %>% filter(Check != '<i class="fas fa-check" role="presentation" aria-label="check icon"></i>')
    # Text display
    paste0("Number of trips analyzed : ", length(trip_select()$trip_id), " ; Number of trip reports :  ", nrow(unique(data_regroup[,c("Vessel code","Trip enddate")]))," ; Number of check : ", length(calcul_check()), " ; Number of check with trip reports :  ", length(unique(data_regroup[,"group_id"])))
      })
  
})
