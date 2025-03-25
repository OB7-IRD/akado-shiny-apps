#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Verification that the user is authorized to connect to AkadoR
  res_auth <- set_server_authentication()

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

  # Read the referential file
  referential_file <- reactive({
    # 0 - Global variables assignement ----
    ID <- NULL
    geometry <- NULL
    # Reading the file with time allocation references by activity
    time_allocation_activity_code_ref <- utils::read.csv(file = system.file("time_allocation_activity_code_ref.csv", package = "furdeb"), header = TRUE, sep = ";")
    # Reading the file with eez shapes
    shape_eez <- sf::read_sf(dsn =  system.file("shp", "EEZ", package = "AkadoR"), layer = "eez_v12_lowres")
    # Correct eez shapes by adding the zone code for "Joint regime area: Senegal / Guinea-Bissau"
    shape_eez[shape_eez$MRGID == 48964, ]$ISO_TER3 <- "XSG"
    shape_eez[shape_eez$MRGID == 48964, ]$TERRITORY3 <- "Joint regime area: Senegal / Guinea-Bissau"
    shape_eez[shape_eez$MRGID == 48964, ]$SOVEREIGN3 <- "Joint regime area: Senegal / Guinea-Bissau"
    # Reading the file with sea shapes
    shape_sea <- sf::read_sf(dsn =  system.file("shp", "World_Seas", package = "AkadoR"), layer = "World_Seas")
    # Grouping of sea pieces
    id_atlantic <- c("15", "15A", "21", "21A", "22", "23", "26", "27", "32", "34")
    id_indian <- c("45", "45A", "44", "46A", "62", "42", "43", "38", "39")
    shape_sea <- dplyr::bind_rows(shape_sea %>%
                                    dplyr::filter(ID %in% id_indian) %>%
                                    dplyr::summarize(geometry = sf::st_union(geometry), ID = "Indian"),
                                  shape_sea %>%
                                    dplyr::filter(ID %in% id_atlantic) %>%
                                    dplyr::summarize(geometry = sf::st_union(geometry), ID = "Atlantic"))
    return(list(time_allocation_activity_code_ref = time_allocation_activity_code_ref, shape_eez = shape_eez, shape_sea = shape_sea))
  })

  # Performs all calculations to test for inconsistencies
  calcul_check <- calcul_check_server(id = "start_button", text_error_trip_select = text_error_trip_select, trip_select = trip_select, config_data = config_data, referential_file = referential_file)

  # Displays the errors and notifications that occur when you want to start the calculation
  error_trip_select_serveur(id = "error_trip_select", text_error_trip_select = text_error_trip_select, config_data = config_data, trip_select = trip_select, calcul_check = calcul_check)

  # Table of consistency test of the presence of activities
  table_server(id = "check_trip_activity", data = calcul_check, number = 1, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of the fishing time
  table_server(id = "check_fishing_time", data = calcul_check, number = 2, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of the sea time
  table_server(id = "check_sea_time", data = calcul_check, number = 3, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of the landing total weight with vessel capacity
  table_server(id = "check_landing_consistent", data = calcul_check, number = 4, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of the total landed weight for canneries is consistent with the weights of each landing for the canneries
  table_server(id = "check_landing_total_weigh", data = calcul_check, number = 5, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of trip start and end date is consistent with the the dates of activity
  table_server(id = "check_temporal_limit", data = calcul_check, number = 6, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Date control plot, display in a window
  output$plot_temporal_limit <- plotly::renderPlotly({
    split_id <- strsplit(input$button_temporal_limit, "&")[[1]]
    data <- eval(parse(text = split_id[[2]]))
    startdate <- as.Date(x = split_id[5], format = "%Y-%m-%d")
    enddate <- as.Date(x = split_id[6], format = "%Y-%m-%d")
    plot_temporal_limit(data, startdate, enddate)
  })

  # Date control window
  observeEvent(input$button_temporal_limit, {
    split_id <- strsplit(input$button_temporal_limit, "&")[[1]]
    vessel_code <- split_id[4]
    enddate <- split_id[6]
    # Non-breaking hyphen (-)
    enddate <- gsub("-", "&#8209;", enddate)
    showModal(modalDialog(
      fluidRow(
        column(3,
          style = "padding-left:5px;padding-right:0px;",
          HTML(paste0("<b>Trip information : </b><br>
                             <ul><li>Vessel code : ", vessel_code, "</li>
                             <li>Trip end date : ", enddate, "</li></ul>"))
        ),
        column(9,
          style = "padding-left:0px;padding-right:5px;",
          plotly::plotlyOutput("plot_temporal_limit")
        )
      ),
      title = "Time coverage detail",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Table of consistency test of the harbour of landing of the previous trip and the harbour of departure of the current trip
  table_server(id = "check_harbour", data = calcul_check, number = 7, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test of the harbour of RF1 and limit values
  table_server(id = "check_raising_factor", data = calcul_check, number = 8, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test school type and association
  table_server(id = "check_fishing_context", data = calcul_check, number = 9, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the succes status and the vessel activity, the type of school or the weight caught
  table_server(id = "check_operationt", data = calcul_check, number = 10, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the ocean declaration and activity position
  table_server(id = "check_position", data = calcul_check, number = 11, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Position control plot, display in a window
  output$plot_position <- plotly::renderPlotly({
    split_id <- strsplit(input$button_position, "&")[[1]]
    data <- eval(parse(text = split_id[[2]]))
    plot_position(data)
  })

  # Position control window
  observeEvent(input$button_position, {
    # Local binding global variables
    . <- NULL
    X <- NULL
    Y <- NULL
    # Split information
    split_id <- strsplit(input$button_position, "&")[[1]]
    enddate <- split_id[5]
    activity_date <- split_id[6]
    # Spatial formatting
    data <- eval(parse(text = split_id[[2]]))
    if (!is.na(data$activity_position)) {
      data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = "activity_crs") %>%
        dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
        dplyr::mutate(X = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate =  Y, latitude = TRUE))
    }else {
      data_geo <- data.frame(Y = c(), X = c())
    }
    # Non-breaking hyphen (-)
    enddate <- gsub("-", "&#8209;", enddate)
    activity_date <- gsub("-", "&#8209;", activity_date)
    showModal(modalDialog(
      fluidRow(
        column(3,
          style = "padding-left:5px;padding-right:0px;",
          HTML(paste0("<b>Trip information : </b><br>
                             <ul><li>Vessel code : ", split_id[4], "</li>
                             <li>Trip end date : ", enddate, "</li>
                             <li>Activity date : ", activity_date, "</li>
                             <li>Activity number : ", split_id[7], "</li>
                             <li>Latitude : ", data_geo$Y, "</li>
                             <li>Longitude : ", data_geo$X, "</li></ul>
                             <b>Problem information : </b><br>
                             <ul><li>Type : ", split_id[8], "</li>
                             <li>Ocean trip : ", split_id[9], "</li>
                             <li>Ocean activity : ", split_id[10], "</li></ul>"))
        ),
        column(9,
          style = "padding-left:0px;padding-right:5px;",
          plotly::plotlyOutput("plot_position")
        )
      ),
      title = "Position",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Table of consistency test the sum of the weight indicated for the catch and activity weight
  table_server(id = "check_weight", data = calcul_check, number = 12, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the sea surface temperature is consistent with valid limits
  table_server(id = "check_temperature", data = calcul_check, number = 15, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the catch weight for activity is consistent with the sample weighting
  table_server(id = "check_weighting_sample", data = calcul_check, number = 16, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the time for route is consistent with the activity
  table_server(id = "check_time_route", data = calcul_check, number = 30, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the eez for activity is consistent with the position
  table_server(id = "check_eez", data = calcul_check, number = 31, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # EEZ control plot, display in a window
  output$plot_eez <- plotly::renderPlotly({
    split_id <- strsplit(input$button_eez, "&")[[1]]
    data <- eval(parse(text = split_id[[2]]))
    plot_eez(data = data, referential_geographical_shape = referential_file()[["shape_eez"]])
  })

  # EEZ control window
  observeEvent(input$button_eez, {
    # Local binding global variables
    . <- NULL
    X <- NULL
    Y <- NULL
    # Split information
    split_id <- strsplit(input$button_eez, "&")[[1]]
    enddate <- split_id[5]
    activity_date <- split_id[6]
    # Spatial formatting
    data <- eval(parse(text = split_id[[2]]))
    if (!is.na(data$activity_position)) {
      data_geo <- sf::st_as_sf(data, wkt = "activity_position", crs = "activity_crs") %>%
        dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
        dplyr::mutate(X = coordinate_dd_to_dmd(coordinate =  X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
    }else {
      data_geo <- data.frame(Y = c(), X = c())
    }
    # Non-breaking hyphen (-)
    enddate <- gsub("-", "&#8209;", enddate)
    activity_date <- gsub("-", "&#8209;", activity_date)
    showModal(modalDialog(
      fluidRow(
        column(3,
          style = "padding-left:5px;padding-right:0px;",
          HTML(paste0("<b>Trip information : </b><br>
                             <ul><li>Vessel code : ", split_id[4], "</li>
                             <li>Trip end date : ", enddate, "</li>
                             <li>Activity date : ", activity_date, "</li>
                             <li>Activity number : ", split_id[7], "</li>
                             <li>Latitude : ", data_geo$Y, "</li>
                             <li>Longitude : ", data_geo$X, "</li></ul>
                             <b>Problem information : </b><br>
                            <ul><li>Declared eez : ", split_id[8], "</li>
                            <li>Declared country eez : ", split_id[9], "</li></ul>"))
        ),
        column(9,
          style = "padding-left:0px;padding-right:5px;",
          plotly::plotlyOutput("plot_eez")
        )
      ),
      title = "EEZ",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Table of consistency test the size class of the samples depending on the species and measurement type is consistent with valid limits
  table_server(id = "check_length_class", data = calcul_check, number = 13, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the total number of individuals measured per sample is consistent with the sum of individuals per sample, species and size class
  table_server(id = "check_measure", data = calcul_check, number = 14, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the species sampled is consistent with species authorized
  table_server(id = "check_species", data = calcul_check, number = 17, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample is consistent with the presence of measurement
  table_server(id = "check_sample_without_measure", data = calcul_check, number = 18, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample is consistent with the presence of species
  table_server(id = "check_sample_without_species", data = calcul_check, number = 19, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample is consistent with the subsample number
  table_server(id = "check_super_sample_number", data = calcul_check, number = 20, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample well number is consistent with the associated trip well numbers
  table_server(id = "check_well_number", data = calcul_check, number = 21, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample is consistent for the percentage of little and big fish sampled
  table_server(id = "check_little_big", data = calcul_check, number = 22, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample is consistent for the weighting
  table_server(id = "check_weighting", data = calcul_check, number = 23, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample weight (m10 and p10) is consistent for the global weight
  table_server(id = "check_weight_sample", data = calcul_check, number = 24, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample and the existence of the activity data_sample_activity
  table_server(id = "check_activity_sample", data = calcul_check, number = 25, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the sample measurement types is consistent for the species or weight
  table_server(id = "check_ldlf", data = calcul_check, number = 26, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the small and large sample weights is consistent for the sum of the small and big weights of the associated well
  table_server(id = "check_distribution", data = calcul_check, number = 27, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Table of consistency test the presence of harbour of landing for sample
  table_server(id = "check_sample_harbour", data = calcul_check, number = 32, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3))

  # Table of consistency test the activity position is consistent for VMS position
  table_server(id = "check_anapo", data = calcul_check, number = 28, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2, 3, 12, 13, 14))

  # Anapo control plot, display in a window
  output$plot_anapo <- plotly::renderPlotly({
    split_id <- strsplit(input$button_anapo, "&")[[1]]
    data_all_click <- strsplit(calcul_check()[[as.numeric(split_id[4])]][as.numeric(split_id[5]), "data_plot"][[1]], "&")[[1]]
    data <- eval(parse(text = data_all_click[[1]]))
    activity_crs <- data_all_click[[3]]
    vms_crs <- data_all_click[[4]]
    activity_data <- eval(parse(text = data_all_click[[5]]))
    trip_data <- eval(parse(text = data_all_click[[6]]))
    plot_anapo(data_vms = data, crs_vms = vms_crs, crs_activity = activity_crs, data_activity = activity_data, data_trip = trip_data)
  })

  # Anapo control window
  observeEvent(input$button_anapo, {
    # Local binding global variables
    X <- NULL
    Y <- NULL
    split_id <- strsplit(input$button_anapo, "&")[[1]]
    data_all_click <- strsplit(calcul_check()[[as.numeric(split_id[4])]][as.numeric(split_id[5]), "data_plot"][[1]], "&")[[1]]
    activity_data <- eval(parse(text = data_all_click[[5]]))
    activity_crs <- data_all_click[[3]]
    # Spatial formatting
    if (!is.na(activity_data$activity_position)) {
      data_geo <- sf::st_as_sf(activity_data, wkt = "activity_position", crs = activity_crs) %>%
        dplyr::mutate(tibble::as_tibble(sf::st_coordinates(.))) %>%
        dplyr::mutate(X = coordinate_dd_to_dmd(coordinate = X, latitude = FALSE), Y = coordinate_dd_to_dmd(coordinate = Y, latitude = TRUE))
    }else {
      data_geo <- data.frame(Y = c(), X = c())
    }
    # Non-breaking hyphen (-)
    enddate <- gsub("-", "&#8209;", activity_data[1, "trip_enddate", drop = TRUE])
    activity_date <- gsub("-", "&#8209;", activity_data[1, "activity_date", drop = TRUE])
    showModal(modalDialog(
      fluidRow(
        column(3,
          style = "padding-left:5px;padding-right:0px;",
          HTML(paste0("<b>Trip information : </b><br>
                             <ul><li>Vessel code : ", activity_data[1, "vessel_code", drop = TRUE], "</li>
                             <li>Trip end date : ", enddate, "</li>
                             <li>Activity date : ", activity_date, "</li>
                             <li>Activity time : ", activity_data[1, "activity_time", drop = TRUE], "</li>
                             <li>Activity number : ", activity_data[1, "activity_number", drop = TRUE], "</li>
                             <li>Vessel activity : ", activity_data[1, "vesselactivity_code", drop = TRUE], "</li>
                             <li>Latitude : ", data_geo$Y, "</li>
                             <li>Longitude : ", data_geo$X, "</li>
                             <li>Grounding : ", activity_data[1, "grounding", drop = TRUE], "</li></ul>"))
        ),
        column(9,
          style = "padding-left:0px;padding-right:5px;",
          plotly::plotlyOutput("plot_anapo")
        )
      ),
      title = "Anapo",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Table of consistency test the VMS is consistent for activity
  table_server(id = "check_anapo_activity", data = calcul_check, number = 33, parent_in = input, text_error_trip_select = text_error_trip_select, trip_select = trip_select, calcul_check = calcul_check, column_no_wrap = c(2))

  # Anapo activity control plot, display in a window
  output$plot_anapo_activity <- plotly::renderPlotly({
    split_id <- strsplit(input$button_anapo_activity, "&")[[1]]
    data_all_click <- strsplit(calcul_check()[[as.numeric(split_id[4])]][as.numeric(split_id[5]), "data_plot"][[1]], "&")[[1]]
    vms_data <- eval(parse(text = data_all_click[[1]]))
    vms_crs <- data_all_click[[4]]
    plot_anapo_activity(data_vms = vms_data, crs_vms = vms_crs, vms_date = data_all_click[[3]])
  })

  # Anapo activity control window
  observeEvent(input$button_anapo_activity, {
    split_id <- strsplit(input$button_anapo_activity, "&")[[1]]
    data_all_click <- strsplit(calcul_check()[[as.numeric(split_id[4])]][as.numeric(split_id[5]), "data_plot"][[1]], "&")[[1]]
    # Non-breaking hyphen (-)
    vms_date <- gsub("-", "&#8209;", data_all_click[[3]])
    showModal(modalDialog(
      fluidRow(
        column(3,
          style = "padding-left:5px;padding-right:0px;",
          HTML(paste0("<b>Trip information : </b><br>
                             <ul><li>Vessel code : ", data_all_click[[5]], "</li>
                             <li>VMS date : ", vms_date, "</li>
                             <li>Vessel type : ", data_all_click[[6]], "</li></ul>"))
        ),
        column(9,
          style = "padding-left:0px;padding-right:5px;",
          plotly::plotlyOutput("plot_anapo_activity")
        )
      ),
      title = "Anapo activity",
      size = "l",
      fade = TRUE,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Management of the display or not of the boxes in the trip tab
  observeEvent(input$type_check_trip, {
    if (input$type_check_trip == "All") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      # Trip
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_fishing_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_fishing_time", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_landing_consistent", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_landing_consistent", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
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
      insertUI(selector = "#div_check_operationt", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_position", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weight", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_temperature", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weighting_sample", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weighting_sample", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weighting_sample", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_time_route", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_eez", anim = TRUE, animType = "fade")
      # Sample
      shinyjs::show(id = "div_check_length_class", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_sample_without_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_super_sample_number", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_well_number", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_little_big", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weighting", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weight_sample", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight_sample", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weight_sample", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_activity_sample", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_ldlf", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_ldlf", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_ldlf", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_distribution", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_sample_harbour", anim = TRUE, animType = "fade")
      # Anapo
      shinyjs::show(id = "div_check_anapo", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_anapo_activity", anim = TRUE, animType = "fade")
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
      shinyjs::hide(id = "div_check_weighting_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_length_class", anim = FALSE)
      shinyjs::hide(id = "div_check_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_species", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_species", anim = FALSE)
      shinyjs::hide(id = "div_check_super_sample_number", anim = FALSE)
      shinyjs::hide(id = "div_check_well_number", anim = FALSE)
      shinyjs::hide(id = "div_check_little_big", anim = FALSE)
      shinyjs::hide(id = "div_check_weighting", anim = FALSE)
      shinyjs::hide(id = "div_check_weight_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_activity_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_ldlf", anim = FALSE)
      shinyjs::hide(id = "div_check_distribution", anim = FALSE)
      shinyjs::hide(id = "div_check_time_route", anim = FALSE)
      shinyjs::hide(id = "div_check_eez", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_harbour", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_raising_factor", anim = TRUE, animType = "fade")
      # Anapo
      shinyjs::hide(id = "div_check_anapo", anim = FALSE)
      shinyjs::hide(id = "div_check_anapo_activity", anim = FALSE)
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
      shinyjs::hide(id = "div_check_weighting_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_length_class", anim = FALSE)
      shinyjs::hide(id = "div_check_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_species", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_measure", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_without_species", anim = FALSE)
      shinyjs::hide(id = "div_check_super_sample_number", anim = FALSE)
      shinyjs::hide(id = "div_check_well_number", anim = FALSE)
      shinyjs::hide(id = "div_check_little_big", anim = FALSE)
      shinyjs::hide(id = "div_check_weighting", anim = FALSE)
      shinyjs::hide(id = "div_check_weight_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_activity_sample", anim = FALSE)
      shinyjs::hide(id = "div_check_ldlf", anim = FALSE)
      shinyjs::hide(id = "div_check_distribution", anim = FALSE)
      shinyjs::hide(id = "div_check_time_route", anim = FALSE)
      shinyjs::hide(id = "div_check_sample_harbour", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_trip_activity", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_landing_consistent", anim = TRUE, time = 1, animType = "fade")
      # Activity
      shinyjs::show(id = "div_check_eez", anim = TRUE, animType = "fade")
      # Anapo
      shinyjs::hide(id = "div_check_anapo", anim = FALSE)
      shinyjs::hide(id = "div_check_anapo_activity", anim = FALSE)
    }
    if (input$type_check_trip == "Error") {
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      shinyjs::hide(id = "div_check_trip_activity", anim = FALSE)
      shinyjs::hide(id = "div_check_landing_consistent", anim = FALSE)
      shinyjs::hide(id = "div_check_raising_factor", anim = FALSE)
      shinyjs::hide(id = "div_check_eez", anim = FALSE)
      # Trip
      shinyjs::show(id = "div_check_fishing_time", anim = TRUE, time = 1, animType = "fade")
      shinyjs::show(id = "div_check_sea_time", anim = TRUE, time = 1, animType = "fade")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_sea_time", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_landing_total_weigh", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_temporal_limit", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_temporal_limit", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_harbour", anim = TRUE, animType = "fade")
      # Activity
      shinyjs::show(id = "div_check_fishing_context", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_operationt", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_operationt", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_operationt", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_position", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weight", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weight", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_temperature", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weighting_sample", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weighting_sample", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weighting_sample", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_time_route", anim = TRUE, animType = "fade")
      # Sample
      shinyjs::show(id = "div_check_length_class", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_measure", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_sample_without_measure", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_sample_without_measure", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_sample_without_species", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_super_sample_number", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_super_sample_number", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_well_number", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_little_big", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_little_big", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_weighting", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_weight_sample", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_weight_sample", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_weight_sample", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_activity_sample", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_ldlf", anim = TRUE, animType = "fade")
      insertUI(selector = "#div_check_ldlf", ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      insertUI(selector = "#div_check_ldlf", ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
      shinyjs::show(id = "div_check_distribution", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_sample_harbour", anim = TRUE, animType = "fade")
      # Anapo
      shinyjs::show(id = "div_check_anapo", anim = TRUE, animType = "fade")
      shinyjs::show(id = "div_check_anapo_activity", anim = TRUE, animType = "fade")
    }
  })

  # Summary page text
  output$text_summary <- renderText({
    # Local binding global variables
    check <- NULL
    # Grouping of data sets with the addition of the group number, then selection of lines containing inconsistencies
    data_regroup <- calcul_check() %>%
      dplyr::bind_rows(.id = "group_id") %>%
      dplyr::filter(check != '<i class="fas fa-check" role="presentation" aria-label="check icon"></i>')
    # Text display
    paste0("Number of trips analyzed : ", length(trip_select()$trip_id), " ; Number of trip reports :  ", nrow(unique(data_regroup[, c("Vessel code", "Trip enddate")])), " ; Number of check : ", length(calcul_check()), " ; Number of check with trip reports :  ", length(unique(data_regroup[, "group_id"])))
  })

  # Date control window
  observeEvent(input$button_download, {
    showModal(window_button_download())
    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste("data-", gsub(" |:|-", "-", Sys.time()), ".csv", sep = "")
      },
      content = function(file) {
        data <- calcul_check()[[input$button_download]]
        # Delete icons
        data[data$Check == "<i class=\"fas fa-check\" role=\"presentation\" aria-label=\"check icon\"></i>", "Check"] <- "TRUE"
        data[data$Check != "TRUE", "Check"] <- "FALSE"
        # Deletes graphics
        if ("Details problem" %in% colnames(data)) {
          data[!is.na(data$`Details problem`), "Details problem"] <- "Detail"
        }
        utils::write.csv(x = data, file = file, row.names = FALSE)
      }
    )
    # Download Excel
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("data-", gsub(" |:|-", "-", Sys.time()), ".xlsx", sep = "")
      },
      content = function(file) {
        data <- calcul_check()[[input$button_download]]
        # Delete icons
        data[data$Check == "<i class=\"fas fa-check\" role=\"presentation\" aria-label=\"check icon\"></i>", "Check"] <- "TRUE"
        data[data$Check != "TRUE", "Check"] <- "FALSE"
        # Deletes graphics
        if ("Details problem" %in% colnames(data)) {
          data[!is.na(data$`Details problem`), "Details problem"] <- "Detail"
        }
        writexl::write_xlsx(x = data, path = file)
      }
    )
  })
}
