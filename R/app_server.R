#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Verification that the user is authorized to connect to AkadoR
  res_auth <- set_server_authentication()

  # Read the referential file
  referential_file <- reactive({
    # 0 - Global variables assignement ----
    ID <- NULL
    geometry <- NULL
    time_at_sea <- NULL
    activity_code_observe <- NULL
    objectoperation_code_observe <- NULL
    fishing_time <- NULL
    # Reading the file with time allocation references by activity
    time_allocation_activity_code_ref <- utils::read.csv(file = system.file("time_allocation_activity_code_ref.csv", package = "furdeb"), header = TRUE, sep = ";")
    # Recovery time allocation references by activity
    vessel_activity_sea_time <- time_allocation_activity_code_ref %>%
      dplyr:: filter(time_at_sea == 1) %>%
      dplyr::mutate(activity_code_observe = as.character(activity_code_observe)) %>%
      dplyr::pull(activity_code_observe)
    object_operation_sea_time <- time_allocation_activity_code_ref %>%
      dplyr:: filter(time_at_sea == 1) %>%
      dplyr::mutate(objectoperation_code_observe = as.character(objectoperation_code_observe)) %>%
      dplyr::pull(objectoperation_code_observe)
    vessel_activity_fishing_time <- time_allocation_activity_code_ref %>%
      dplyr:: filter(fishing_time == 1) %>%
      dplyr::mutate(activity_code_observe = as.character(activity_code_observe)) %>%
      dplyr::pull(activity_code_observe)
    object_operation_fishing_time <- time_allocation_activity_code_ref %>%
      dplyr:: filter(fishing_time == 1) %>%
      dplyr::mutate(objectoperation_code_observe = as.character(objectoperation_code_observe)) %>%
      dplyr::pull(objectoperation_code_observe)
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
    names_referential <- c("vessel_activity_sea_time", "object_operation_sea_time", "vessel_activity_fishing_time", "object_operation_fishing_time", "shape_eez", "shape_sea")
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
  # function_check : Function that performs the control and creates the result table, mandatory, function expected
  # argument_function_check : List of arguments for function function_check (the left part is the name given to the arguments in the function_check, the right part is the content of the arguments, which may come from SQL (indicate the name of the file), or from a parameter in the configuration file, or from the SQL initializing the user's query trip_select() (trip_selected, vms, vms_route or activity_vms), or from reference files referential_file() (vessel_activity_sea_time, object_operation_sea_time, vessel_activity_fishing_time, object_operation_fishing_time, shape_eez or shape_sea), or a parameter to be hardened), mandatory, character expected
  # table_user_id : Name of the SQL containing the information needed for the user to identify the rows (which may come from SQL (indicate the name of the file) or from the SQL initializing the user's query trip_select() (trip_selected, vms, vms_route or activity_vms)), mandatory, character expected
  # user_type : Name of user type authorized to execute this check, mandatory, character expected
  # additional_column_user : Names of additional table_user_id columns to be displayed for user accounting, optional, character expected
  # function_display : Function for adding modifications to the specific table for check, optional, function expected
  # argument_function_display : List of arguments for function function_display (the left part is the name given to the arguments in the function_display, the right part is the content of the arguments, which may come from SQL (indicate the name of the file), or 'check' to pass the first element of the check output, or 'plot' to pass the second element of the check output, or a parameter to be hardened), optional (default : pass the first element of the check output), character expected
  # rename_column_user : List of column names to be renamed (the left part is the column names, the right part is the new names), optional, character expected
  # need_vms : logical which indicates if VMS data are used in the check, then the check is not executed if the connection information to the VMS database is missing, optional (default : FALSE), logical expected
  # column_no_wrap : Column numbers that should not be subject to automatic line breaks, optional, integer expected
  # size_box : Check size specification for each window size type (format "col-sm-your_size col-md-your_size col-lg-your_size"), optional, character expected
  # function_data_plot : Function to create data from the result of the control to be supplied to function_plot (must create a list to fill the arguments of function_plot, the left part is the name given to the arguments in the function_plot, the right part is the content of the arguments), optional, function expected
  # argument_function_data_plot : List of arguments for function function_data_plot (the left part is the name given to the arguments in the function_data_plot, the right part is the content of the arguments, which may come from the SQL initializing the user's query trip_select() (trip_selected, vms, vms_route or activity_vms), or 'check' to pass the check output, or 'plot' to pass only the second element of the check output, or a parameter to be hardened), optional (but mandatory if function_data_plot exist), character expected
  # choice_display_plot : argument to select the lines that will display a plot (cf function data_button_plot, argument choice_select_row), optional, character expected
  # function_plot : Function that creates the plot, optional, function expected
  # function_text_plot : Function that creates the text to be displayed in the plot window, optional, function expected
  # title_window : Plot window name, optional, character expected
  check_info <- list(list(id = "check_trip_activity",
                          tab = "trip",
                          title = "Presence of activity",
                          type = "warning",
                          function_check = check_trip_activity_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "activity", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_fishing_time",
                          tab = "trip",
                          title = "Fishing time",
                          text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Fishing Time' field of the trip</li></ul>",
                          type = "error",
                          function_check = check_fishing_time_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "route", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          rename_column_user = list(trip_fishingtime = "Trip fishing time", sum_route_fishingtime = "Sum route fishing time"),
                          column_no_wrap = c(2)),
                     list(id = "check_sea_time",
                          tab = "trip",
                          title = "Sea time",
                          text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Sea Time' field of the trip</li></ul>",
                          type = "error",
                          function_check = check_sea_time_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "route", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          rename_column_user = list(trip_seatime = "Trip sea time", sum_route_seatime = "Sum route sea time"),
                          column_no_wrap = c(2)),
                     list(id = "check_landing_consistent",
                          tab = "trip",
                          title = "Vessel capacity",
                          text = "<ul><li>If the total landed weight is greater than the vessel's capacity, you must verify that the 'landed weight' is correct</li></ul>",
                          type = "warning",
                          function_check = check_landing_consistent_inspector,
                          argument_function_check = list(dataframe1 = "trip", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          rename_column_user = list(vessel_capacity = "Vessel capacity", trip_weighttotal = "Total weight"),
                          column_no_wrap = c(2)),
                     list(id = "check_landing_total_weigh",
                          tab = "trip",
                          title = "Total landed weight",
                          text = "<ul><li>If the values are not equal, you must enter the value of the sum of the commercial lots in the 'Landed Weight' field of the trip</li></ul>",
                          type = "error",
                          function_check = check_landing_total_weight_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "landing", output = "report", epsilon = "epsilon"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          rename_column_user = list(trip_landingtotalweight = "Trip landing weight", sum_weightlanding = "Sum landing weight"),
                          column_no_wrap = c(2)),
                     list(id = "check_temporal_limit",
                          tab = "trip",
                          title = "Time coverage",
                          text = "<ul><li>You must check the fishing log to see if a day is missing</li>
                                  <li>You must verify that the departure and arrival dates match in the fishing logbook and landing documents</li></ul>",
                          type = "error",
                          function_check = check_temporal_limit_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "route", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          column_no_wrap = c(2),
                          function_data_plot = plot_temporal_limit_data,
                          argument_function_data_plot = list(dataframe1 = "plot", dataframe2 = "trip"),
                          function_plot = plot_temporal_limit,
                          function_text_plot = plot_temporal_limit_windows,
                          title_window = "Time coverage detail"),
                     list(id = "check_harbour",
                          tab = "trip",
                          title = "Harbour",
                          text = "<ul><li>Check if all fishing logs have been entered</li>
                                  <li>Check with the captain to see if any trip have been made in the meantime</li></ul>",
                          type = "error",
                          function_check = check_harbour_inspector,
                          argument_function_check = list(dataframe1 = "previous_trip", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          rename_column_user = list(harbour_label_landing_trip_previous = "Previous harbour landing", harbour_label_departure = "Harbour departure"),
                          column_no_wrap = c(2)),
                     list(id = "check_raising_factor",
                          tab = "trip",
                          title = "Raising Factor",
                          text = "<ul><li>If the ratio is not between 0.9 < R < 1.1 (Landing/catch), you need to check the partial landing value</li></ul>",
                          type = "info",
                          function_check = check_raising_factor_inspector,
                          argument_function_check = list(dataframe1 = "trip", dataframe2 = "catch_full_trip", dataframe3 = "landing_full_trip", dataframe4 = "full_trip", output = "report"),
                          table_user_id = "trip",
                          user_type = c("logbook"),
                          additional_column_user = c("wellcontentstatus_landing_label"),
                          rename_column_user = list(wellcontentstatus_landing_label = "Landing well status", rf1 = "RF1"),
                          function_display = display_raising_factor,
                          column_no_wrap = c(2)),
                     list(id = "check_fishing_context",
                          tab = "activity",
                          title = "Fishing context",
                          text = "<ul><li>If the school type is \"object school\" (code 1), then there must be at least one object-type association</li>
                                  <li>If the school type is \"free school\" (code 2), then the association identifier, if it exists, must not be of object type</li></ul>",
                          type = "error",
                          function_check = check_fishing_context_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "activity_observedsystem", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          rename_column_user = list(schooltype_code = "School type", association_object_count = "Number of associations object"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_operation",
                          tab = "activity",
                          title = "Operation",
                          text = "<ul><li>If the vessel activity is \"Fishing (skiff is deployed)\" (code 6), then it needs a success status, otherwise it must not have any</li>
                                  <li>If the school type is \"Undefined\" (code 0), success type must not be \"Positive\" (code 1) or \"Null\" (code 0)</li>
                                  <li>If the weight is not 0 then success type must be \"Positive\" (code 1) or \"Unknown\" (code 2), otherwise, if it exists, must be 0</li></ul>",
                          type = "error",
                          function_check = check_operation_inspector,
                          argument_function_check = list(dataframe1 = "activity", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          rename_column_user = list(schooltype_code = "School type", successstatus_code = "Success status", activity_weight = "Weigth"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_position",
                          tab = "activity",
                          title = "Position",
                          text = "<ul><li>If the position is on land, you need to check the latitude and longitude</li>
                                  <li>If the position and ocean are different, you need to check these fields with the logbook</li></ul>",
                          type = "error",
                          function_check = check_position_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "trip", dataframe3 = "shape_sea", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          additional_column_user = c("activity_position"),
                          rename_column_user = list(type = "Type", ocean_label = "Ocean trip", ocean_calculate = "Ocean activity", activity_position_ddm = "Position activity"),
                          column_no_wrap = c(2, 3),
                          function_data_plot = plot_position_data,
                          argument_function_data_plot = list(dataframe1 = "plot", dataframe2 = "activity"),
                          function_plot = plot_position,
                          function_text_plot = plot_position_windows,
                          title_window = "Position"),
                     list(id = "check_weight",
                          tab = "activity",
                          title = "Total Catch Weight",
                          text = "<ul><li>If the values are different, you must transfer the sum of the elementary captures to the activity</li></ul>",
                          type = "error",
                          function_check = check_weight_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "catch", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          rename_column_user = list(activity_weight = "Activity weight", sum_catch_weight = "Sum catch weight"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_temperature",
                          tab = "activity",
                          title = "Temperature",
                          text = "<ul><li>Indicates whether the temperature is between 15 and 32 for the activity</li></ul>",
                          type = "error",
                          function_check = check_temperature_inspector,
                          argument_function_check = list(dataframe1 = "activity", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          rename_column_user = list(activity_seasurfacetemperature = "Sea surface temperature"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_weighting_sample",
                          tab = "activity",
                          title = "Weighting sample",
                          text = "<ul><li>Indicates whether the sum of the catch weights for the activity is consistent with the sum of the sample weighted weights link for the activity</li></ul>",
                          type = "error",
                          function_check = check_weighting_sample_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "catch", dataframe3 = "sampleactivity", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          rename_column_user = list(weight = "Sum catch weight", weightedweight = "Sum sample weighted weight"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_time_route",
                          tab = "activity",
                          title = "Time route",
                          text = "<ul><li>Indicates whether sea time and fishing time exceed their maximum limits (default 24 for sea time and 13 for fishing time).</li>
                                  <li>Indicates whether sea time is less than fishing time</li><li>Indicates whether sea time equals 0 while sea activities are associated</li>
                                  <li>Indicates whether fishing time equals 0 while fishing activities are associated</li></ul>",
                          type = "error",
                          function_check = check_time_route_inspector,
                          argument_function_check = list(dataframe1 = "route", dataframe2 = "activity", dataframe3 = "floatingobject", vessel_activity_sea_time = "vessel_activity_sea_time", object_operation_sea_time = "object_operation_sea_time", vessel_activity_fishing_time = "vessel_activity_fishing_time", object_operation_fishing_time = "object_operation_fishing_time", output = "report"),
                          table_user_id = "route",
                          user_type = c("logbook"),
                          rename_column_user = list(route_seatime = "Sea time", route_fishingtime = "Fishing time", nb_activity_must_seatime = "Number activities must sea time", nb_activity_must_fishingtime = "Number activities must fishing time"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_eez",
                          tab = "activity",
                          title = "EEZ",
                          text = "<ul><li>Indicates when the declared and calculated eez are different</li>
                                  <li>Indicates when there is no declared eez for fishing activities </li></ul>",
                          type = "warning",
                          function_check = check_eez_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "shape_eez", output = "report"),
                          table_user_id = "activity",
                          user_type = c("logbook"),
                          additional_column_user = c("activity_position"),
                          rename_column_user = list(fpazone_code = "EEZ", fpazone_country_iso3 = "Country EEZ", eez_calculated = "Calculated EEZ", activity_position_ddm = "Position activity"),
                          column_no_wrap = c(2, 3),
                          function_data_plot = plot_eez_data,
                          argument_function_data_plot = list(dataframe1 = "plot", dataframe2 = "activity"),
                          choice_display_plot = "all",
                          function_plot = plot_eez,
                          function_text_plot = plot_eez_windows,
                          title_window = "EEZ"),
                     list(id = "check_length_class",
                          tab = "sample",
                          title = "Size class",
                          text = "<ul><li>Indicates FL measurements of species 'YFT', 'BET', 'ALB' greater than 80</li></ul>",
                          type = "error",
                          function_check = check_length_class_inspector,
                          argument_function_check = list(dataframe1 = "samplespeciesmeasure", output = "report"),
                          table_user_id = "samplespeciesmeasure",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_measure",
                          tab = "sample",
                          title = "Measurement",
                          text = "<ul><li>Indicates per sample whether the sum of individuals measured in the species samples is different from the sum of individuals measured in the size classes</li></ul>",
                          type = "error",
                          function_check = check_measure_inspector,
                          argument_function_check = list(dataframe1 = "samplespecies", dataframe2 = "samplespeciesmeasure", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sum_measuredcount = "Sum numbers individuals sample species", sum_count = "Sum numbers individuals size class"),
                          column_no_wrap = c(2)),
                     list(id = "check_species",
                          tab = "sample",
                          title = "Species",
                          text = "<ul><li>Indicates species not conforming with respect to the following list:'YFT', 'SKJ', 'BET', 'ALB', 'LTA', 'FRI', 'TUN', 'KAW', 'LOT'</li></ul>",
                          type = "error",
                          function_check = check_species_inspector,
                          argument_function_check = list(dataframe1 = "samplespecies", output = "report"),
                          table_user_id = "samplespecies",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_sample_without_measure",
                          tab = "sample",
                          title = "Sample without measurement",
                          text = "<ul><li>Indicates species samples that have no size measurement</li></ul>",
                          type = "error",
                          function_check = check_sample_without_measure_inspector,
                          argument_function_check = list(dataframe1 = "samplespecies", dataframe2 = "samplespeciesmeasure", output = "report"),
                          table_user_id = "samplespecies",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_sample_without_species",
                          tab = "sample",
                          title = "Sample without species",
                          text = "<ul><li>Indicates samples that have no species samples</li></ul>",
                          type = "error",
                          function_check = check_sample_without_species_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "samplespecies", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_super_sample_number",
                          tab = "sample",
                          title = "Super sample",
                          text = "<ul><li>If the sample is a super sample, then there must be several sub-samples, numbered from 1</li><li>Otherwise there must be a single sub-sample, numbered 0</li></ul>",
                          type = "error",
                          function_check = check_super_sample_number_consistent_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "samplespecies", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_supersample = "Super sample", count_subsamplenumber_n0 = "Counts number sub-sample numbers not 0", count_subsamplenumber_0 = "Counts number sub-sample numbers equal 0", count_subsamplenumber_1 = "Counts number sub-sample numbers equal 1", count_subsamplenumber = "Counts number different sub-sample numbers"),
                          column_no_wrap = c(2)),
                     list(id = "check_well_number",
                          tab = "sample",
                          title = "Well",
                          text = "<ul><li>The sample well must exist in the trip well plan</li></ul>",
                          type = "error",
                          function_check = check_well_number_consistent_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "well", dataframe3 = "trip", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_well = "Well"),
                          column_no_wrap = c(2)),
                     list(id = "check_little_big",
                          tab = "sample",
                          title = "Ratio of small over big fish",
                          text = "<ul><li>The percentages of small over big fish must be consistent within the sample</li></ul>",
                          type = "error",
                          function_check = check_little_big_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "samplespecies", dataframe3 = "samplespeciesmeasure", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          function_display = display_little_big,
                          rename_column_user = list(sample_smallsweight = "Small fish weight", sample_bigsweight = "Big fish weight", sample_totalweight = "Total weight", little_percentage = "Little %", big_percentage = "Big %", measure1_percentage = "Measurement type FL %", measure2_percentage = "Measurement type PD1 %"),
                          column_no_wrap = c(2)),
                     list(id = "check_weighting",
                          tab = "sample",
                          title = "Weighting",
                          text = "<ul><li>The weighting for each sample must be coherent</li></ul>",
                          type = "error",
                          function_check = check_weighting_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "sampleactivity", dataframe3 = "trip", dataframe4 = "landing", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_smallsweight = "Small fish weight", sample_bigsweight = "Big fish weight", sample_totalweight = "Total weight", sampletype_code = "Sample type", weightedweight = "Sum weighted weights", vesseltype_label = "Vessel type", sum_landing_weight_baitboat = "Sum weight fresh landings baitboats"),
                          column_no_wrap = c(2)),
                     list(id = "check_weight_sample",
                          tab = "sample",
                          title = "Weight sample",
                          text = "<ul><li>Only one value must be entered between the total weight and the count of the weight of small or big fish</li></ul>",
                          type = "error",
                          function_check = check_weight_sample_inspector,
                          argument_function_check = list(dataframe1 = "sample", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_smallsweight = "Small fish weight", sample_bigsweight = "Big fish weight", sample_totalweight = "Total weight"),
                          column_no_wrap = c(2)),
                     list(id = "check_activity_sample",
                          tab = "sample",
                          title = "Sample activity",
                          text = "<ul><li>Sample must be linked to an activity</li></ul>",
                          type = "error",
                          function_check = check_activity_sample_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "sampleactivity", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_ldlf",
                          tab = "sample",
                          title = "LDLF",
                          text = "<ul><li>The type of measure must be compatible with species</li>
                                  <li>The type of measurement must be compatible with total, small and big fish weights</li></ul>",
                          type = "error",
                          function_check = check_ldlf_inspector,
                          argument_function_check = list(dataframe1 = "samplespecies", dataframe2 = "sample", output = "report"),
                          table_user_id = "samplespecies",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_smallsweight = "Small fish weight", sample_bigsweight = "Big fish weight", sample_totalweight = "Total weight"),
                          column_no_wrap = c(2)),
                     list(id = "check_category_species_forbidden_well",
                          tab = "sample",
                          title = "Category prohibited for some species in wells",
                          text = "<ul><li>Samples must not have certain species (default 'SKJ') associated with certain weight categories (default 'W-9', i.e. unknown)</li></ul>",
                          type = "warning",
                          function_check = check_category_species_forbidden_well_inspector,
                          argument_function_check = list(dataframe1 = "wellactivityspecies", output = "report"),
                          table_user_id = "wellactivityspecies",
                          user_type = c("logbook"),
                          column_no_wrap = c(2)),
                     list(id = "check_distribution",
                          tab = "sample",
                          title = "Distribution +10/-10",
                          text = "<ul><li>Sample must have -10/+10 distribution values consistent with those of the reported well</li>
                                  <li>The weight of the big category for the well includes by default only the following species 'YFT', 'BET', 'ALB', 'SKJ'</li>
                                  <li>The small weight category for the well includes by default only the following species 'YFT', 'BET', 'ALB', 'SKJ' plus the unknown weight category for species 'SKJ'</li></ul>",
                          type = "error",
                          function_check = check_distribution_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "well", dataframe3 = "wellactivity", dataframe4 = "wellactivityspecies", output = "report", species_category_small_big = "list_species_reference_well_distribution_control"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(sample_smallsweight = "Small fish weight", sample_bigsweight = "Big fish weight", sample_well = "Well", weight_sum_small_filter = "Small fish weight well filter", weight_sum_big_filter = "Big fish weight well filter", weight_sum_small = "Small fish weight well", weight_sum_big = "Big fish weight well"),
                          column_no_wrap = c(2)),
                     list(id = "check_sample_harbour",
                          tab = "sample",
                          title = "Sample harbour",
                          text = "<ul><li>Samples must have a harbour of landing</li></ul>",
                          type = "error",
                          function_check = check_sample_harbour_inspector,
                          argument_function_check = list(dataframe1 = "sample", dataframe2 = "trip", output = "report"),
                          table_user_id = "sample",
                          user_type = c("logbook"),
                          rename_column_user = list(harbour_label_landing = "Harbour landing"),
                          column_no_wrap = c(2, 3)),
                     list(id = "check_anapo",
                          tab = "anapo",
                          title = "Anapo",
                          text = "<ul><li>There must be at least 20 VMS positions during the day</li>
                          <li>There must be at least one VMS position nearer than 10 miles away OR the score (resulting from geographical and temporal distance) must be greater than or equal to 0.5 OR the position must be in a harbour</li></ul>",
                          type = "error",
                          function_check = check_anapo_inspector,
                          argument_function_check = list(dataframe1 = "activity", dataframe2 = "trip", dataframe3 = "vms", output = "report"),
                          function_data_plot = plot_anapo_data,
                          argument_function_data_plot = list(dataframe1 = "plot", dataframe2 = "activity", dataframe3 = "transmittingbuoy"),
                          function_display = display_anapo,
                          argument_function_display = list(dataframe1 = "plot", dataframe2 = "activity", dataframe3 = "transmittingbuoy", dataframe4 = "check"),
                          need_vms = TRUE,
                          table_user_id = "activity",
                          user_type = c("vms_logbook"),
                          rename_column_user = list(nb_vms = "Number of VMS", min_distance = "Minimale distance", max_score = "Maximum score", grounding = "Grounding", activity_position_ddm = "Position activity", activity_position_prior_ddm = "Position previous activity (not grounding)", activity_position_post_ddm = "Position next activity (not grounding)"),
                          size_box = "col-sm-12 col-md-12 col-lg-12",
                          column_no_wrap = c(2, 3, 12, 13, 14),
                          choice_display_plot = "all",
                          function_plot = plot_anapo,
                          function_text_plot = plot_anapo_windows,
                          title_window = "Anapo"),
                     list(id = "check_anapo_activity",
                          tab = "anapo",
                          title = "Anapo activity",
                          text = "<ul><li>Each VMS must have at least one existing activity, for vessel types seiner (large and without bait (5,6)), bait boat (freezer and ice (1,2)) and supply (10)</li></ul>
                                  (Warning: in case of inconsistency all vessels (Vessel code) linked to the VMS vessel code are displayed inconsistently, select only active vessels (Vessel status 1) if you are not working on historical data)",
                          type = "error",
                          function_check = check_anapo_activity_consistent_inspector,
                          argument_function_check = list(dataframe1 = "vms_route", dataframe2 = "activity_vms", output = "report"),
                          function_data_plot = plot_anapo_activity_data,
                          argument_function_data_plot = list(dataframe1 = "check", dataframe2 = "vms"),
                          need_vms = TRUE,
                          table_user_id = "vms_route",
                          user_type = c("vms_logbook"),
                          additional_column_user = c("vessel_code", "vms_id", "vms_date", "vms_codevessel", "vessel_type", "vessel_statut"),
                          rename_column_user = list(nb_activity = "Number activities", vessel_type = "Vessel type", vessel_statut = "Vessel statut", vms_codevessel = "VMS vessel code", vms_date = "VMS date"),
                          size_box = "col-sm-12 col-md-12 col-lg-12",
                          column_no_wrap = c(2),
                          function_plot = plot_anapo_activity,
                          function_text_plot = plot_anapo_activity_windows,
                          title_window = "Anapo activity"))
  # Information about the sql
  # file : Name of sql file (without .sql extension), mandatory, character expected
  # anchor : list containing information in case of anchor in sql file, the left part is the name given to the anchor in the SQL, the right part is the content of the anchor, which may come from another SQL (indicate the name of the file, note that use_selection_other_sql must be TRUE for this one and FALSE for the current SQL), or from a parameter in the configuration file, or from the SQL initializing the user's query (trip_selected or vessel_selected), optional, character expected
  # use_selection_other_sql : Indicates whether the SQL is used in the construction of other SQLs, optional (default : FALSE), logical expected
  # column_anchor : Name of column to be used to supply values when used in other SQL anchors, optional (but mandatory if use_selection_other_sql is TRUE and vector is FALSE), character expected
  # column_user_id : Column names used by the user to identify the element, optional, character expected
  # vector : Logical which indicates whether the data frame created should be transformed into a vector (if a single column in the data frame), optional (default : FALSE), logical expected
  sql_info <- list(list(file = "trip_selected"), # Do not modify, different generated directly from user inputs
                   list(file = "vms"), # Do not modify, different generated directly from user inputs
                   list(file = "activity_vms"), # Do not modify, different generated directly from user inputs
                   list(file = "activity",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = TRUE,
                        column_anchor = "activity_id",
                        column_user_id = c("activity_id", "vessel_code", "trip_enddate", "activity_date", "activity_time", "activity_number", "vesselactivity_code")),
                   list(file = "sample",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = TRUE,
                        column_anchor = "sample_id",
                        column_user_id = c("sample_id", "vessel_code", "trip_enddate", "sample_number")),
                   list(file = "samplespecies",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE,
                        column_user_id = c("samplespecies_id", "vessel_code", "trip_enddate", "sample_number", "samplespecies_subsamplenumber", "species_fao_code", "sizemeasuretype_code")),
                   list(file = "samplespeciesmeasure",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE,
                        column_user_id = c("samplespeciesmeasure_id", "vessel_code", "trip_enddate", "sample_number", "samplespecies_subsamplenumber", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass")),
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
                        anchor = list(select_item_1 = "trip_selected"),
                        use_selection_other_sql = FALSE,
                        column_user_id = c("trip_id", "vessel_code", "trip_enddate")),
                   list(file = "previous_trip",
                        anchor = list(select_item_1 = "logbook_program",
                                      select_item_2 = "trip_selected",
                                      select_item_3 = "vessel_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "wellactivity",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE),
                   list(file = "wellactivityspecies",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE,
                        column_user_id = c("wellactivityspecies_id", "vessel_code", "trip_enddate", "well_label", "weightcategory_code", "species_fao_code")),
                   list(file = "route",
                        anchor = list(select_item = "trip_selected"),
                        use_selection_other_sql = FALSE,
                        column_user_id = c("route_id", "vessel_code", "trip_enddate", "activity_date")),
                   list(file = "full_trip",
                        anchor = list(select_item_1 = "logbook_program",
                                      select_item_2 = "trip_selected",
                                      select_item_3 = "vessel_selected"),
                        use_selection_other_sql = TRUE,
                        column_anchor = "trip_id"),
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
                        use_selection_other_sql = FALSE,
                        vector = TRUE))
  # Information about the column user
  # rename_id_column_user : List of column names to be renamed (the left part is the column names of column_user_id in sql_info, the right part is the new names), mandatory, character expected
  # order_id_column_user : Column names of column_user_id in sql_info, according to which rows are to be sorted, mandatory, character expected
  column_user_info <- list(rename_id_column_user = list(vessel_code = "Vessel code", trip_enddate = "Trip enddate", activity_date = "Activity date",  activity_time = "Activity time", activity_number = "Activity number", vesselactivity_code = "Vessel activity", sample_number = "Sample number", samplespecies_subsamplenumber = "Sub sample number", species_fao_code = "FAO code", sizemeasuretype_code = "Size measure type", samplespeciesmeasure_sizeclass = "Size class", well_label = "Well", weightcategory_code = "Weight category"),
                           order_id_column_user = c("vessel_code", "trip_enddate"))

  # Checks the consistency of the various lists before filtering by the user
  check_consistency_list(sql_info = sql_info, check_info = check_info, column_user_info = column_user_info, type_check_info = type_check_info, tab_info = tab_info)

  # Error message if the date range is not correct
  output$error_date_select <- renderText({
    if (isTruthy(input$trip_start_date_range) && isTruthy(input$trip_end_date_range) && input$trip_start_date_range > input$trip_end_date_range) {
      "Error: start date must be before end date"
    }
  })

  # Read the .yml file of configuration for the connection
  config_data <- config_data_server(id = "start_button", parent_in = input)

  # Filter check by user selection
  check_info_selected <- reactive({
    check_info[sapply(check_info, function(check) any(check[["id"]] %in% input[["tab-select_check"]]))]
  })

  # Filter SQL by user selection
  sql_info_selected <- reactive({
    # Retrieves arguments that may contain references to SQL data
    name_argument_check_select <- unique(unname(unlist(sapply(check_info_selected(), `[`, c("argument_function_check", "argument_function_data_plot", "argument_function_display", "table_user_id")))))
    # Retrieves the SQLs info used in the anchors for the selected SQL data
    sql_info_check_select <- sql_info[sapply(sql_info, function(sql) any(sql[["file"]] %in% name_argument_check_select))]
    # Filter SQL by user selection
    sql_info <- sql_info[sapply(sql_info, function(sql) any(sql[["file"]] %in% c(name_argument_check_select, unname(unlist(sapply(sql_info_check_select, `[[`, "anchor"))))))]
    # Deletion of SQLs that are managed differently, with the use of user inputs
    sql_info_input_user <- sql_info
    sql_info <- sql_info[!sapply(sql_info, function(sql) any(sql[["file"]] %in% c("trip_selected", "vms", "activity_vms")))]
    # Add default value FALSE for use_selection_other_sql
    for (sql in sql_info) {
      if (is.null(sql[["use_selection_other_sql"]])) {
        sql_info[[which(sapply(sql_info, `[[`, "file") == sql[["file"]])]][["use_selection_other_sql"]] <- FALSE
      }
    }
    return(list(sql_info_input_user = sql_info_input_user, sql_info = sql_info))
  })

  # Error message if the trip selection elements are not correctly filled in
  text_error_trip_select <- text_error_trip_select_server(id = "start_button", parent_in = input, config_data = config_data)

  # Retrieves the list of trips selected by the user
  trip_select <- trip_select_server(id = "start_button", parent_in = input, text_error_trip_select = text_error_trip_select, config_data = config_data, sql_info_selected = sql_info_selected)

  # Performs all calculations to test for inconsistencies
  calcul_check <- calcul_check_server(id = "start_button", text_error_trip_select = text_error_trip_select, trip_select = trip_select, config_data = config_data, referential_file = referential_file, check_info_selected = check_info_selected, sql_info_selected = sql_info_selected, column_user_info = column_user_info, parent_in = input)

  # Displays the errors and notifications that occur when you want to start the calculation
  error_trip_select_serveur(id = "error_trip_select", text_error_trip_select = text_error_trip_select, config_data = config_data, trip_select = trip_select, calcul_check = calcul_check)

  # Tab creation, menu, tab content
  tab(id = "tab", tab_info = tab_info, check_info = check_info, type_check_info = type_check_info, calcul_check = calcul_check, referential_file = referential_file, config_data = config_data, res_auth = res_auth)

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
