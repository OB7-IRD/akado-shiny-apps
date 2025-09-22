#' @title tab_content UI Function
#' @description A shiny Module for creation of all tab
#' @param id Internal parameters for shiny
#' @return The function returns a {\link[shiny]{tagList}} with a {\link[shinydashboard]{tabItems}} and {\link[shinydashboard]{tabItem}}
#' @export
mod_tab_content_ui <- function(id) {
  ns <- shiny::NS(namespace = id)
  shiny::tagList(
    uiOutput(ns("content"))
  )
}

#' @title tab_content Server Functions
#' @description A shiny Module for creation of all tab
#' @param id Internal parameters for shiny
#' @param tab_info {\link[base]{list}} expected. Reactive value contains information about the dynamic tab display allowed for user type
#' @param check_info {\link[base]{list}} expected. Reactive value contains information about the dynamic check display allowed for user type
#' @param type_check_info {\link[base]{list}} expected. Information about the dynamic type of check
#' @param config_data {\link[base]{list}} expected. Reactive value contains the contents of the configuration file
#' @param res_auth {\link[base]{character}} expected. Reactive value containing information about the user's connection
#' @param parameters_trip_select {\link[base]{data.frame}} expected. Reactive value containing information about the trip store in database
#' @return The function returns nothing, instantiating the tab
#' @details
#' \itemize{
#' tab_info:
#' \item{ tab_info_authorize : }
#'    \itemize{
#'    \item{\code{  id : Tab identification, mandatory, character expected}}
#'    }
#' }
#' \itemize{
#' check_info:
#'    \itemize{
#'    \item{ id_check_authorize : Check identification, mandatory, character expected}
#'    \item{ check_info_authorize : }
#'      \itemize{
#'      \item{\code{  id : Check identification, mandatory, character expected}}
#'      \item{\code{  tab : Tab identification, mandatory, character expected}}
#'      \item{\code{  title : Name to be displayed for check, optional, character expected}}
#'      \item{\code{  text : Text to be displayed for check, optional, character expected}}
#'      \item{\code{  type : Check type, you must choose between check type identifiers (type_check_info), mandatory, character expected}}
#'      \item{\code{  column_no_wrap : Column numbers that should not be subject to automatic line breaks, optional, integer expected}}
#'      \item{\code{  size_box : Check size specification for each window size type (format "col-sm-your_size col-md-your_size col-lg-your_size"), optional, character expected}}
#'      }
#'    }
#'}
#' \itemize{
#' type_check_info:
#'  \item{\code{  id : Button selection identification, mandatory, character expected}}
#'  \item{\code{  specific_check : Indicates whether the selection should display specific controls or not, optional (default : TRUE), logical expected}}
#' }
#' \itemize{
#' config_data:
#'  \item{\code{  databases_configuration : named list with database connection information}}
#' }
#' \itemize{
#' parameters_trip_select:
#'  \item{\code{  trip_id }}
#'  \item{\code{  vessel_active }}
#'  \item{\code{  vessel_status }}
#'  \item{\code{  trip_startdate }}
#'  \item{\code{  trip_enddate }}
#'  \item{\code{  vessel_code }}
#'  \item{\code{  vessel_label }}
#'  \item{\code{  ocean_id }}
#'  \item{\code{  ocean_label }}
#'  \item{\code{  ocean_code }}
#'  \item{\code{  flagcountry_id }}
#'  \item{\code{  flagcountry_label }}
#'  \item{\code{  flagcountry_code }}
#'  \item{\code{  fleetcountry_id }}
#'  \item{\code{  fleetcountry_label }}
#'  \item{\code{  fleetcountry_code }}
#'  \item{\code{  logbookprogram_id }}
#'  \item{\code{  logbookprogram_label }}
#'  \item{\code{  logbookprogram_code }}
#' }
#' @export
mod_tab_content_server <- function(id, tab_info, check_info, type_check_info, config_data, res_auth, parameters_trip_select) {
  moduleServer(id, function(input, output, session) {
    # Creation of all tab, both static tab and user-specified dynamic tab (static tabs are also created here, as the UI does not support the mixing of dynamic and static tabs)
    output$content <- shiny::renderUI({
      # Creation of all tab
      items <- c(
        # Creating the static home tab
        list(shinydashboard::tabItem(
          tabName = "home",
          fluidPage(
            shinydashboard::box(
              width = 12,
              title = "Overall context",
              status = "primary",
              solidHeader = TRUE,
              p("AKADO automatically performs a series of tests on the data and produces summary tables that provides a more or less detailed assessment of the anomalies detected. A final summary presents the percentage of occurrences of errors that remains to be corrected.")
            ),
            shinydashboard::box(
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              title = "Selection data base Observe",
              selectInput(inputId = NS(namespace = id, id = "data_base_observe"), label = "Choose a data base:", choices = list("No database defined for this user, contact your administrator"), multiple = TRUE),
              column(width = 2, offset = 5, align = "center",
                actionButton(inputId = NS(namespace = "start_data_base", id = "start_data_base"), label = "Load trip selection parameters"),
                shinycssloaders::withSpinner(htmlOutput(NS(namespace = "error_data_base_select", id = "text")), type = 6, size = 0.5, proxy.height = "70px")
              ),
            ),
            shinydashboard::box(
              width = 12,
              title = "Selection of the trip ",
              status = "primary",
              solidHeader = TRUE,
              fluidRow(
                align = "center",
                style = "margin-left: 0px; margin-right: 0px;",
                shiny::tagAppendAttributes(
                  shinyWidgets::materialSwitch(inputId = NS(namespace = id, id = "select_several_trip"), label = "Select of 1 trip", status = "primary", inline = TRUE),
                  style = "text-align: initial; margin-bottom: 0px;"
                ),
                "Select of several trips"
              ),
              fluidRow(
                align = "center",
                style = "margin-left: 0px; margin-right: 0px;",
                checkboxInput(inputId = NS(namespace = id, id = "vessel_active"), label = "Active vessels only", value = TRUE)
              ),
              fluidRow(
                tags$style(".class_disable_parameter {background-color: #eee; opacity: 0.2;}"),
                column(width = 6, id = NS(namespace = id, id = "column_parameter_one_trip"),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "one_trip"), label = "Choose a trip:", choices = list("No parameter defined, load trip selection parameters"))
                ),
                column(width = 6, id = NS(namespace = id, id = "column_parameter_several_trip"),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "vessel"), label = "Choose a vessel:", choices = list("No parameter defined, load trip selection parameters"), multiple = TRUE),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "ocean"), label = "Choose a ocean:", choices = list("No parameter defined, load trip selection parameters"), multiple = TRUE),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "flag"), label = "Choose a flag country:", choices = list("No parameter defined, load trip selection parameters"), multiple = TRUE),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "fleet"), label = "Choose a fleet country:", choices = list("No parameter defined, load trip selection parameters"), multiple = TRUE),
                  shinyWidgets::slimSelectInput(inputId = NS(namespace = id, id = "program"), label = "Choose a program:", choices = list("No parameter defined, load trip selection parameters"), multiple = TRUE),
                  sliderInput(inputId = NS(namespace = id, id = "range_enddate"), label = "Choose an end date range:", min = Sys.Date(), max = Sys.Date(), value = c(Sys.Date(), Sys.Date())),
                  style = "border-left: 1px solid gray;"
                )
              ),
              fluidRow(
                style = "display: flex; align-items: center; flex-wrap: wrap; border-top: 1px dashed gray; margin-left: 0px; margin-right: 0px; margin-top: 10px;",
                column(width = 2, offset = 5, align = "center",
                       actionButton(inputId = NS(namespace = "start_button", id = "start_button"), label = "Start"),
                       shinycssloaders::withSpinner(htmlOutput(NS(namespace = "error_trip_select", id = "text")), type = 6, size = 0.5, proxy.height = "70px")),
                column(width = 5,
                       style = "margin-top: 10px;",
                       selectInput(inputId = NS(namespace = id, id = "select_check"), label = "Choose a check (optional):", choices = check_info()[["id_check_authorize"]], multiple = TRUE, selected = check_info()[["id_check_authorize"]]))
              )
            )
          )
        )),
        # Creating dynamic tab
        # Use of lapply and not a for loop to avoid lazy evaluation problems
        lapply(
          tab_info()[["tab_info_authorize"]],
          function(tab) {
            shinydashboard::tabItem(
              tabName = tab[["id"]],
              fluidPage(
                lapply(
                  check_info()[["check_info_authorize"]],
                  function(check) {
                    # Filters the checks available for the tab
                    if (check[["tab"]] == tab[["id"]]) {
                      # Configure display check with recovered parameters
                      do.call(mod_table_ui, check[names(check) %in% c("id", "title", "text", "size_box")])
                    }
                  }
                )
              )
            )
          }
        ),
        # Creating the static summary tab
        list(shinydashboard::tabItem(
          tabName = "summary",
          fluidPage(
            textOutput("text_summary")
          )
        ),
        # Creating the static setting tab
        shinydashboard::tabItem(
          tabName = "setting",
          fluidPage(
            # Launching the application without authentication or administrator login
            if (is.null(res_auth) || res_auth[["admin"]] == TRUE) {
              fileInput(
                inputId = "setting_file_path", label = paste0("Path of the configuration file .yml ; (Default: ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"), ")"),
                multiple = FALSE,
                accept = c(".yml")
              )
            } else {
              # Not administrator login
              HTML(paste0("<span style=\"color:red\">", "Only the AkadoR administrator can change the session configuration file", "</span>"))
            }
          )
        ))
      )
      do.call(shinydashboard::tabItems, items)
    })

    # Initializes database selection memory
    choices_data_base_observe <- reactiveValues(old_value = "", current_value = "")

    # Update choice for database selection Observe
    observeEvent(config_data(), {
      # Saves old selection options
      choices_data_base_observe$old_value <- choices_data_base_observe$current_value
      # Defines the options for database selection Observe
      if (is.null(config_data())) {
        choices_data_base_observe$current_value <- "No configuration file"
      } else {
        # Launching the application without authentication
        if (is.null(res_auth)) {
          choices_data_base_observe$current_value <- names(config_data()[["databases_configuration"]])
        } else {
          # Checks that the BD names is specified in configuration file
          if (!all(unlist(strsplit(res_auth[["database_user_types"]], ";")) %in% names(config_data()[["databases_configuration"]]))) {
            warning(
              format(
                x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"
              ),
              " - At least one database ", res_auth[["database_user_types"]], " for user ", res_auth[["user"]], " is not defined in the configuration file.\n",
              sep = ""
            )
          }
          # Lists the possible database choices for this user
          choices_data_base_observe$current_value <- unlist(strsplit(res_auth[["database_user_types"]], ";"))
        }
      }
      # If database are change in config_data
      if (!identical(unlist(choices_data_base_observe$current_value), character(0)) && (length(choices_data_base_observe$old_value) != length(choices_data_base_observe$current_value) || any(choices_data_base_observe$old_value != choices_data_base_observe$current_value))) {
        # Updated options for database selection Observe
        updateSelectInput(session, "data_base_observe", choices = choices_data_base_observe$current_value)
      }
    })

    # Update widget section parameters from database(s)
    observeEvent(c(parameters_trip_select(), input$vessel_active), {
      # 0 - Global variables assignement ----
      vessel_status <- NULL
      trip_startdate <- NULL
      vessel_label <- NULL
      ocean_label <- NULL
      flagcountry_label <- NULL
      fleetcountry_label <- NULL
      logbookprogram_label <- NULL
      if (is.data.frame(parameters_trip_select())) {
        # Widget trip section parameters
        trip <- parameters_trip_select()
        # Filter vessel active
        if (input$vessel_active) {
          trip <- trip %>% dplyr::filter(vessel_status == 1)
        }
        # Sort rows
        trip <- trip %>% dplyr::arrange(dplyr::desc(trip_startdate), vessel_label)
        # Improves display
        list_one_trip <- stats::setNames(as.list(trip[["trip_id"]]), paste0(trip[["trip_startdate"]], " - ", trip[["trip_enddate"]], " - ", trip[["vessel_label"]], " - ", trip[["vessel_code"]]))
        # Updated options for on trip selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "one_trip", choices = list_one_trip)
        rm(list_one_trip)
        # Widget vessel section parameters
        vessel <- trip[, c("vessel_id", "vessel_label", "vessel_code")]
        # Remove duplicate row
        vessel <- unique(vessel)
        # Sort rows
        vessel <- vessel %>% dplyr::arrange(vessel_label)
        # Improves display
        list_vessel <- stats::setNames(as.list(vessel[["vessel_id"]]), paste0(vessel[["vessel_label"]], " - ", vessel[["vessel_code"]]))
        rm(vessel)
        # Updated options for on vessel selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "vessel", choices = list_vessel)
        rm(list_vessel)
        # Widget ocean section parameters
        ocean <- trip[, c("ocean_id", "ocean_label", "ocean_code")]
        # Remove duplicate row
        ocean <- unique(ocean)
        # Sort rows
        ocean <- ocean %>% dplyr::arrange(ocean_label)
        # Improves display
        list_ocean <- stats::setNames(as.list(ocean[["ocean_id"]]), paste0(ocean[["ocean_label"]], " - ", ocean[["ocean_code"]]))
        rm(ocean)
        # Updated options for on ocean selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "ocean", choices = list_ocean)
        rm(list_ocean)
        # Widget flag section parameters
        flag <- trip[, c("flagcountry_id", "flagcountry_label", "flagcountry_code")]
        # Remove duplicate row
        flag <- unique(flag)
        # Sort rows
        flag <- flag %>% dplyr::arrange(flagcountry_label)
        # Improves display
        list_flag <- stats::setNames(as.list(flag[["flagcountry_id"]]), paste0(flag[["flagcountry_label"]], " - ", flag[["flagcountry_code"]]))
        rm(flag)
        # Updated options for on flag selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "flag", choices = list_flag)
        rm(list_flag)
        # Widget fleet section parameters
        fleet <- trip[, c("fleetcountry_id", "fleetcountry_label", "fleetcountry_code")]
        # Remove duplicate row
        fleet <- unique(fleet)
        # Sort rows
        fleet <- fleet %>% dplyr::arrange(fleetcountry_label)
        # Improves display
        list_fleet <- stats::setNames(as.list(fleet[["fleetcountry_id"]]), paste0(fleet[["fleetcountry_label"]], " - ", fleet[["fleetcountry_code"]]))
        rm(fleet)
        # Updated options for on fleet selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "fleet", choices = list_fleet)
        rm(list_fleet)
        # Widget program section parameters
        program <- trip[, c("logbookprogram_id", "logbookprogram_label", "logbookprogram_code")]
        # Remove duplicate row
        program <- unique(program)
        # Sort rows
        program <- program %>% dplyr::arrange(logbookprogram_label)
        # Improves display
        list_program <- stats::setNames(as.list(program[["logbookprogram_id"]]), paste0(program[["logbookprogram_label"]], " - ", program[["logbookprogram_code"]]))
        rm(program)
        # Updated options for on program selection
        shinyWidgets::updateSlimSelect(session = session, inputId = "program", choices = list_program)
        rm(list_program)
        # Widget range enddate section parameters
        min_range_enddate <- min(trip$trip_enddate)
        max_range_enddate <- max(trip$trip_enddate)
        updateSliderInput(session = session, inputId = "range_enddate", min = min_range_enddate, max = max_range_enddate, value = c(min_range_enddate, max_range_enddate))
        rm(trip)
      }
    })

    # Enables or disables widgets depending on the trip type selected by the user
    observeEvent(input$select_several_trip, {
      if (input$select_several_trip) {
        # Disables the settings column for one trip and enables it for several trips
        shinyjs::disable(id = "column_parameter_one_trip")
        shinyjs::runjs(paste0("$('#", id, "-column_parameter_one_trip').addClass('class_disable_parameter')"))
        shinyjs::enable(id = "column_parameter_several_trip")
        shinyjs::runjs(paste0("$('#", id, "-column_parameter_several_trip').removeClass('class_disable_parameter')"))
      } else {
        # Disables the settings column for several trips and enables it for one trip
        shinyjs::disable(id = "column_parameter_several_trip")
        shinyjs::runjs(paste0("$('#", id, "-column_parameter_several_trip').addClass('class_disable_parameter')"))
        shinyjs::enable(id = "column_parameter_one_trip")
        shinyjs::runjs(paste0("$('#", id, "-column_parameter_one_trip').removeClass('class_disable_parameter')"))
      }
    })

    # Management of the display or not of the boxes in the trip tab
    observeEvent(input$type_check, {
      # Information on the type of control selected by the user, its name and whether it concerns all types of control or not
      specific_check <- lapply(type_check_info, function(check) {
        if (is.list(check) && check[["id"]] == input$type_check) {
          check[["specific_check"]]
        }
      })
      specific_check <- unlist(specific_check)
      if (is.null(specific_check)) {
        specific_check <- TRUE
      }
      removeUI(selector = "div:has(> #div_visible_md_check)", multiple = TRUE)
      removeUI(selector = "div:has(> #div_visible_lg_check)", multiple = TRUE)
      for (tab in tab_info()[["tab_info_authorize"]]) {
        # Number of the control displayed within the tab, to display a line for every two controls
        num_check <- 0
        for (check in check_info()[["check_info_authorize"]]) {
          # Filters the checks available for the tab
          if (check[["tab"]] == tab[["id"]]) {
            # Controls the display of check according to the type chosen by the user
            if (specific_check && check[["type"]] != input$type_check) {
              # Hide controls
              shinyjs::hide(id = paste0("div_", check[["id"]]), anim = FALSE, asis = TRUE)
            } else {
              # Displaying controls
              shinyjs::show(id = paste0("div_", check[["id"]]), anim = TRUE, time = 1, animType = "fade", asis = TRUE)
              num_check <- num_check + 1
              # Display thin lines to separate control lines for md and lg window sizes
              if (num_check %% 2 == 0 & num_check > 0 & ((!is.null(tab[["display_dividing_lines"]]) && tab[["display_dividing_lines"]]) || (is.null(tab[["display_dividing_lines"]]) && TRUE))) {
                # Displays a horizontal line every two controls if the tab allows it and there are at least 3 controls in the tab
                insertUI(selector = paste0("#div_", check[["id"]]), ui = div(div(class = "clearfix visible-md", id = "div_visible_md_check"), div(class = "visible-md", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
                insertUI(selector = paste0("#div_", check[["id"]]), ui = div(div(class = "clearfix visible-lg", id = "div_visible_lg_check"), div(class = "visible-lg", hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #F4F4F4, #333, #F4F4F4); background-image: -moz-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4); background-image: -ms-linear-gradient(left,#F4F4F4, #9A9A9A, #F4F4F4); background-image: -o-linear-gradient(left, #F4F4F4, #9A9A9A, #F4F4F4);"))), where = "afterEnd")
              }
            }
          }
        }
      }
    })
  })
}

## To be copied in the UI
# mod_tab_content_ui("tab_content_1")

## To be copied in the server
# mod_tab_content_server("tab_content_1", tab_info, check_info, type_check_info, config_data, res_auth)
