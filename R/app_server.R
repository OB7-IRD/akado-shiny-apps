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
    # Correct eez shapes by adding the zone code for "Ile Tromelin"
    shape_eez[shape_eez$MRGID == 48946, ]$ISO_TER2 <- "MDG"
    shape_eez[shape_eez$MRGID == 48946, ]$ISO_TER3 <- "MUS"
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
    names_referential <- c("time_allocation_activity_code_ref", "shape_eez", "shape_sea")
    return(stats::setNames(mget(names_referential), names_referential))
  })

  # Information about the radio button to select check
  # title : Button title, mandatory, character expected
  # id : Button selection identification, mandatory, character expected
  # text : Display text for button selection, mandatory, character expected
  # specific_check : Indicates whether the selection should display specific controls or not, optional (default : TRUE), logical expected
  type_check_info <- list(title = "Choose the display of verification type",
                          list(id = "all",
                               text = "All (Info, warning or error)",
                               specific_check = FALSE),
                          list(id = "info",
                               text = "Info only",
                               specific_check = TRUE),
                          list(id = "warning",
                               text = "Warning only",
                               specific_check = TRUE),
                          list(id = "error",
                               text = "Error only",
                               specific_check = TRUE))
  # Information about the dynamic tab display
  # id : Tab identification, mandatory, character expected
  # title : Name to be displayed for tab, mandatory, character expected
  # icon : Picture to be displayed for tab, optional, character expected
  # display_dividing_lines : Whether or not to display separator lines between check, optional (default : TRUE), logical expected
  tab_info <- list(list(id = "trip",
                        title = "Trip",
                        icon = "ship",
                        display_dividing_lines = TRUE),
                   list(id = "activity",
                        title = "Activity",
                        icon = "list-check",
                        display_dividing_lines = TRUE),
                   list(id = "sample",
                        title = "Sample",
                        icon = "fish",
                        display_dividing_lines = TRUE),
                   list(id = "anapo",
                        title = "Anapo",
                        icon = "route",
                        display_dividing_lines = FALSE))
  # Check display information (Attention controls must be in the same order as the desired display)
  # id : Check identification, mandatory, character expected
  # tab : Tab identification, mandatory, character expected
  # title : Name to be displayed for check, optional, character expected
  # text : Text to be displayed for check, optional, character expected
  # type : Check type, you must choose between check type identifiers (type_check_info), mandatory, character expected
  # column_no_wrap : Column numbers that should not be subject to automatic line breaks, optional, integer expected
  # size_box : Check size specification for each window size type (format "col-sm-your_size col-md-your_size col-lg-your_size"), optional, character expected
  # name_function_plot : Name of the function that creates the plot, optional, character expected
  # name_function_text_plot : Name of the function that creates the text to be displayed in the plot window, optional, character expected
  # title_window : Plot window name, optional, character expected
  check_info <- list(list(id = "check_trip_activity",
                          tab = "trip",
                          title = "Presence of activity",
                          type = "warning",
                          column_no_wrap = c(2)),
                     list(id = "check_fishing_time",
                          tab = "trip",
                          title = "Fishing time",
                          text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Fishing Time' field of the trip</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_sea_time",
                          tab = "trip",
                          title = "Sea time",
                          text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Sea Time' field of the trip</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_landing_consistent",
                          tab = "trip",
                          title = "Vessel capacity",
                          text = "<ul><li>If the total landed weight is greater than the vessel's capacity, you must verify that the 'landed weight' is correct</li></ul>",
                          type = "warning",
                          column_no_wrap = c(2)),
                     list(id = "check_landing_total_weigh",
                          tab = "trip",
                          title = "Total landed weight",
                          text = "<ul><li>If the values are not equal, you must enter the value of the sum of the commercial lots in the 'Landed Weight' field of the trip</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_temporal_limit",
                          tab = "trip",
                          title = "Time coverage",
                          text = "<ul><li>You must check the fishing log to see if a day is missing</li>
                                  <li>You must verify that the departure and arrival dates match in the fishing logbook and landing documents</li></ul>",
                          type = "error",
                          column_no_wrap = c(2),
                          name_function_plot = "plot_temporal_limit",
                          name_function_text_plot = "plot_temporal_limit_windows",
                          title_window = "Time coverage detail"),
                     list(id = "check_harbour",
                          tab = "trip",
                          title = "Harbour",
                          text = "<ul><li>Check if all fishing logs have been entered</li>
                                  <li>Check with the captain to see if any trip have been made in the meantime</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_raising_factor",
                          tab = "trip",
                          title = "Raising Factor",
                          text = "<ul><li>If the ratio is not between 0.9 < R < 1.1 (Landing/catch), you need to check the partial landing value</li></ul>",
                          type = "info",
                          column_no_wrap = c(2)),
                     list(id = "check_fishing_context",
                          tab = "activity",
                          title = "Fishing context",
                          text = "<ul><li>If the school type is \"object school\" (code 1), then there must be at least one object-type association</li>
                                  <li>If the school type is \"free school\" (code 2), then the association identifier, if it exists, must not be of object type</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_operation",
                          tab = "activity",
                          title = "Operation",
                          text = "<ul><li>If the vessel activity is \"Fishing (skiff is deployed)\" (code 6), then it needs a success status, otherwise it must not have any</li>
                                  <li>If the school type is \"Undefined\" (code 0), success type must not be \"Positive\" (code 1) or \"Null\" (code 0)</li>
                                  <li>If the weight is not 0 then success type must be \"Positive\" (code 1) or \"Unknown\" (code 2), otherwise, if it exists, must be 0</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_position",
                          tab = "activity",
                          title = "Position",
                          text = "<ul><li>If the position is on land, you need to check the latitude and longitude</li>
                                  <li>If the position and ocean are different, you need to check these fields with the logbook</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3),
                          name_function_plot = "plot_position",
                          name_function_text_plot = "plot_position_windows",
                          title_window = "Position"),
                     list(id = "check_weight",
                          tab = "activity",
                          title = "Total Catch Weight",
                          text = "<ul><li>If the values are different, you must transfer the sum of the elementary captures to the activity</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_temperature",
                          tab = "activity",
                          title = "Temperature",
                          text = "<ul><li>Indicates whether the temperature is between 15 and 32 for the activity</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_weighting_sample",
                          tab = "activity",
                          title = "Weighting sample",
                          text = "<ul><li>Indicates whether the sum of the catch weights for the activity is consistent with the sum of the sample weighted weights link for the activity</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_time_route",
                          tab = "activity",
                          title = "Time route",
                          text = "<ul><li>Indicates whether sea time and fishing time exceed their maximum limits (default 24 for sea time and 13 for fishing time).</li>
                                  <li>Indicates whether sea time is less than fishing time</li><li>Indicates whether sea time equals 0 while sea activities are associated</li>
                                  <li>Indicates whether fishing time equals 0 while fishing activities are associated</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_eez",
                          tab = "activity",
                          title = "EEZ",
                          text = "<ul><li>Indicates when the declared and calculated eez are different</li>
                                  <li>Indicates when there is no declared eez for fishing activities </li></ul>",
                          type = "warning",
                          column_no_wrap = c(2, 3),
                          name_function_plot = "plot_eez",
                          name_function_text_plot = "plot_eez_windows",
                          title_window = "EEZ"),
                     list(id = "check_length_class",
                          tab = "sample",
                          title = "Size class",
                          text = "<ul><li>Indicates FL measurements of species 'YFT', 'BET', 'ALB' greater than 80</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_measure",
                          tab = "sample",
                          title = "Measurement",
                          text = "<ul><li>Indicates per sample whether the sum of individuals measured in the species samples is different from the sum of individuals measured in the size classes</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_species",
                          tab = "sample",
                          title = "Species",
                          text = "<ul><li>Indicates species not conforming with respect to the following list:'YFT', 'SKJ', 'BET', 'ALB', 'LTA', 'FRI', 'TUN', 'KAW', 'LOT'</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_sample_without_measure",
                          tab = "sample",
                          title = "Sample without measurement",
                          text = "<ul><li>Indicates species samples that have no size measurement</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_sample_without_species",
                          tab = "sample",
                          title = "Sample without species",
                          text = "<ul><li>Indicates samples that have no species samples</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_super_sample_number",
                          tab = "sample",
                          title = "Super sample",
                          text = "<ul><li>If the sample is a super sample, then there must be several sub-samples, numbered from 1</li><li>Otherwise there must be a single sub-sample, numbered 0</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_well_number",
                          tab = "sample",
                          title = "Well",
                          text = "<ul><li>The sample well must exist in the trip well plan</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_little_big",
                          tab = "sample",
                          title = "Ratio of small over big fish",
                          text = "<ul><li>The percentages of small over big fish must be consistent within the sample</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_weighting",
                          tab = "sample",
                          title = "Weighting",
                          text = "<ul><li>The weighting for each sample must be coherent</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_weight_sample",
                          tab = "sample",
                          title = "Weight sample",
                          text = "<ul><li>Only one value must be entered between the total weight and the count of the weight of small or big fish</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_activity_sample",
                          tab = "sample",
                          title = "Sample activity",
                          text = "<ul><li>Sample must be linked to an activity</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_ldlf",
                          tab = "sample",
                          title = "LDLF",
                          text = "<ul><li>The type of measure must be compatible with species</li>
                                  <li>The type of measurement must be compatible with total, small and big fish weights</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_category_species_forbidden_well",
                          tab = "sample",
                          title = "Category prohibited for some species in wells",
                          text = "<ul><li>Samples must not have certain species (default 'SKJ') associated with certain weight categories (default 'W-9', i.e. unknown)</li></ul>",
                          type = "warning",
                          column_no_wrap = c(2)),
                     list(id = "check_distribution",
                          tab = "sample",
                          title = "Distribution +10/-10",
                          text = "<ul><li>Sample must have -10/+10 distribution values consistent with those of the reported well</li>
                                  <li>The weight of the big category for the well includes by default only the following species 'YFT', 'BET', 'ALB', 'SKJ'</li>
                                  <li>The small weight category for the well includes by default only the following species 'YFT', 'BET', 'ALB', 'SKJ' plus the unknown weight category for species 'SKJ'</li></ul>",
                          type = "error",
                          column_no_wrap = c(2)),
                     list(id = "check_sample_harbour",
                          tab = "sample",
                          title = "Sample harbour",
                          text = "<ul><li>Samples must have a harbour of landing</li></ul>",
                          type = "error",
                          column_no_wrap = c(2, 3)),
                     list(id = "check_anapo",
                          tab = "anapo",
                          title = "Anapo",
                          text = "<ul><li>There must be at least 20 VMS positions during the day</li>
                          <li>There must be at least one VMS position nearer than 10 miles away OR the score (resulting from geographical and temporal distance) must be greater than or equal to 0.5 OR the position must be in a harbour</li></ul>",
                          type = "error",
                          size_box = "col-sm-12 col-md-12 col-lg-12",
                          column_no_wrap = c(2, 3, 12, 13, 14),
                          name_function_plot = "plot_anapo",
                          name_function_text_plot = "plot_anapo_windows",
                          title_window = "Anapo"),
                     list(id = "check_anapo_activity",
                          tab = "anapo",
                          title = "Anapo activity",
                          text = "<ul><li>Each VMS must have at least one existing activity, for vessel types seiner (large and without bait (5,6)), bait boat (freezer and ice (1,2)) and supply (10)</li></ul>
                                  (Warning: in case of inconsistency all vessels (Vessel code) linked to the VMS vessel code are displayed inconsistently, select only active vessels (Vessel status 1) if you are not working on historical data)",
                          type = "error",
                          size_box = "col-sm-12 col-md-12 col-lg-12",
                          column_no_wrap = c(2),
                          name_function_plot = "plot_anapo_activity",
                          name_function_text_plot = "plot_anapo_activity_windows",
                          title_window = "Anapo activity"))
  # Information about the sql
  # file : Name of sql file (without .sql extension), mandatory, character expected
  # anchor : list containing information in case of anchor in sql file, the left part is the name given to the anchor in the SQL, the right part is the content of the anchor, which may come from another SQL (indicate the name of the file, note that use_selection_other_sql must be TRUE for this one), or from a parameter in the configuration file, or from the SQL initializing the user's query (trip_selected or vessel_selected), optional, character expected
  # use_selection_other_sql : Indicates whether the SQL is used in the construction of other SQLs, optional (default : FALSE), logical expected
  # column : Name of column to be used to supply values when used in other SQL anchors, optional (but mandatory if use_selection_other_sql is TRUE), character expected
  sql_info <- list(list(file = "activity",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = TRUE,
                        column = "activity_id"),
                   list(file = "sample",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = TRUE,
                        column = "sample_id"),
                   list(file = "samplespecies",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "samplespeciesmeasure",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "well",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "landing",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "sampleactivity",
                        anchor = list(select_item = "sample"),
                        use_selection_other_sql = FALSE),
                   list(file = "trip",
                        anchor = list(select_item_1 = "logbook_program",
                                      select_item_2 = "trip_selected",
                                      select_item_3 = "vessel_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "wellactivity",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "wellactivityspecies",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "route",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "full_trip",
                        anchor = list(select_item_1 = "logbook_program",
                                      select_item_2 = "trip_selected",
                                      select_item_3 = "vessel_selected"),
                        use_selection_other_sql = TRUE,
                        column = "trip_id"),
                   list(file = "catch_full_trip",
                        anchor = list(select_item = "full_trip"),
                        use_selection_other_sql = FALSE),
                   list(file = "landing_full_trip",
                        anchor = list(select_item = "full_trip"),
                        use_selection_other_sql = FALSE),
                   list(file = "activity_observedsystem",
                        anchor = list(select_item = "activity"),
                        use_selection_other_sql = FALSE),
                   list(file = "catch",
                        anchor = list(select_item = "activity"),
                        use_selection_other_sql = FALSE),
                   list(file = "transmittingbuoy",
                        anchor = list(select_item = "activity"),
                        use_selection_other_sql = FALSE),
                   list(file = "floatingobject",
                        anchor = list(select_item = "activity"),
                        use_selection_other_sql = FALSE),
                   list(file = "list_species_reference_well_distribution_control",
                        anchor = list(select_item = "reference_list_species_well_distribution_control"),
                        use_selection_other_sql = FALSE))

  # Performs all calculations to test for inconsistencies
  calcul_check <- calcul_check_server(id = "start_button", text_error_trip_select = text_error_trip_select, trip_select = trip_select, config_data = config_data, referential_file = referential_file, sql_info = sql_info)

  # Displays the errors and notifications that occur when you want to start the calculation
  error_trip_select_serveur(id = "error_trip_select", text_error_trip_select = text_error_trip_select, config_data = config_data, trip_select = trip_select, calcul_check = calcul_check)

  # Tab creation, menu, tab content
  tab(id = "tab", tab_info = tab_info, check_info = check_info, type_check_info = type_check_info, calcul_check = calcul_check, referential_file = referential_file)

  # Force activation of first tab at startup, remove the lazy evaluation
  observe({
    if (is.null(input$sidebarmenu_id)) {
      shinydashboard::updateTabItems(session, "sidebarmenu_id", selected = "home")
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
