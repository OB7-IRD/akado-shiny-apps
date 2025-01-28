#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Add logo
  header <- shinydashboard::dashboardHeader(
    title = "AKaDo",
    disable = FALSE,
    titleWidth = 550
  )
  header$children[[2]]$children[[1]] <- list(
    tags$a(tags$img(src = file.path("www", "favicon.png"), height = "100%", align = "left"), target = "_blank"),
    header$children[[2]]$children[[1]]
  )
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Application UI
    shinydashboard::dashboardPage(
      header,
      shinydashboard::dashboardSidebar(
        width = 200,
        shinydashboard::sidebarMenu(
          style = "overflow: visible",
          shinydashboard::menuItem("Home", tabName = "home", icon = icon("house")),
          shinydashboard::menuItem("Trip", tabName = "trip", icon = icon("ship")),
          shinydashboard::menuItem("Activity", tabName = "activity", icon = icon("list-check")),
          shinydashboard::menuItem("Sample", tabName = "sample", icon = icon("fish")),
          shinydashboard::menuItem("Anapo", tabName = "anapo", icon = icon("route")),
          shinydashboard::menuItem("Summary", tabName = "summary", icon = icon("scroll")),
          hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #333, #ccc, #333); background-image: -moz-linear-gradient(left, #333, #ccc, #333); background-image: -ms-linear-gradient(left,#333, #ccc, #333); background-image: -o-linear-gradient(left, #333, #ccc, #333);"),
          radioButtons(inputId = "type_check_trip", label = "Choose the display of verification type", choiceNames = c("All (Info, warning or error)", "Info only", "Warning only", "Error only"), choiceValues = list("All", "Info", "Warning", "Error")),
          radioButtons(inputId = "type_line_check_trip", label = "Choose the display of line type", choiceNames = list(HTML(paste0("All ( ", icon("check"), " - ", icon("info"), " - ", icon("exclamation"), " - ", icon("xmark"), " )")), HTML(paste0("Info, warning or error ( ", icon("info"), " - ", icon("exclamation"), " - ", icon("xmark"), " )"))), choiceValues = list("All", "inconsistent")),
          shinydashboard::menuItem("Settings", tabName = "setting", icon = icon("gear"))
        )
      ),
      shinydashboard::dashboardBody(
        # Allows you to hide or show the boxes according to
        shinyjs::useShinyjs(),
        # Fix the setting onget at the bottom
        tags$head(
          tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    "))
        ),
    # Fix the scrollbar with withSpinner effect
    tags$head(tags$style(
      HTML(".wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}")
    )),
    # Color of the icons in relation to the consistency test
    tags$style(".fa-check {color:#05DE1E}"),
    tags$style(".fa-exclamation {color:#FFA600}"),
    tags$style(".fa-xmark {color:#DE0505}"),
    tags$style(".fa-info {color:#F4D03F}"),
    # Green background color for notifications id = "notif_default"
    tags$style("#shiny-notification-notif_default {background-color:#B2E8A2;}"),
    # Font size buttons in menu
    tags$style("#type_check_trip, #type_line_check_trip {font-size:11px;}"),
    # Changes paddings between columns in display tables
    tags$style(HTML("table.dataTable thead>tr>th.sorting{
      padding-left:2px;
      padding-right:10px;
      }")),
    # Change position of sort icon in display tables
    tags$style(HTML("table.dataTable thead>tr>th.sorting:before,
      table.dataTable thead>tr>th.sorting:after{
      right: 0px
      }")),
    # Changes the size of the plot
    tags$style(
      type = "text/css",
      ".modal-dialog {width:75% !important; }"
    ),
    # Add pages
    shinydashboard::tabItems(
      shinydashboard::tabItem(
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
            width = 6,
            title = "Selection of the trip ",
            status = "primary",
            solidHeader = TRUE,
            shinydashboard::box(
              width = 12,
              title = "Unique selection by vessel number and trip end date",
              solidHeader = TRUE,
              fluidRow(
                column(width = 6, numericInput(
                  inputId = "vessel_number", label = "Indicate a vessel number:", value = NA, min = 0
                )),
                column(width = 6, dateInput(
                  inputId = "trip_end_date", label = HTML("Choose a trip end date:&nbsp&nbsp&nbsp&nbsp&nbsp"), value = NA
                ))
              )
            ),
            shinydashboard::box(
              width = 12,
              title = "Multiple selection by date range",
              solidHeader = TRUE,
              fluidRow(
                column(width = 6, dateInput(
                  inputId = "trip_start_date_range", label = "Choose a trip start date:", value = NA
                )),
                column(width = 6, dateInput(
                  inputId = "trip_end_date_range", label = HTML("Choose a trip end date:&nbsp&nbsp"), value = NA
                ))
              )
            ),
            span(textOutput("error_date_select"), style = "color:red;")
          ),
          shinydashboard::box(
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            title = "Data base Observe",
            selectInput(
              inputId = "data_base_observe", label = "Choose a data base:",
              choices = list("---", "NY", "NJ", "CT")
            )
          ),
          shinydashboard::box(
            width = 12,
            align = "center",
            status = "primary",
            actionButton(inputId = NS(namespace = "start_button", id = "start_button"), label = "Start"),
            shinycssloaders::withSpinner(htmlOutput(NS(namespace = "error_trip_select", id = "text")), type = 6, size = 0.5, proxy.height = "70px")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "trip",
        fluidPage(
          table_ui(id = "check_trip_activity", title = "Presence of activity"),
          table_ui(id = "check_fishing_time", title = "Fishing time", text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Fishing Time' field of the trip</li></ul>"),
          table_ui(id = "check_sea_time", title = "Sea time", text = "<ul><li>If the values are not equivalent, you must enter the sum in the 'Sea Time' field of the trip</li></ul>"),
          table_ui(id = "check_landing_consistent", title = "Vessel capacity", text = "<ul><li>If the total landed weight is greater than the vessel's capacity, you must verify that the 'landed weight' is correct</li></ul>"),
          table_ui(id = "check_landing_total_weigh", title = "Total landed weight", text = "<ul><li>If the values are not equal, you must enter the value of the sum of the commercial lots in the 'Landed Weight' field of the trip</li></ul>"),
          table_ui(id = "check_temporal_limit", title = "Time coverage", text = "<ul><li>You must check the fishing log to see if a day is missing</li>
                 <li>You must verify that the departure and arrival dates match in the fishing logbook and landing documents</li></ul>"),
          table_ui(id = "check_harbour", title = "Harbour", text = "<ul><li>Check if all fishing logs have been entered</li>
                 <li>Check with the captain to see if any trip have been made in the meantime</li></ul>"),
          table_ui(id = "check_raising_factor", title = "Raising Factor", text = "<ul><li>If the ratio is not between 0.9 < R < 1.1 (Landing/catch), you need to check the partial landing value</li></ul>")
        )
      ),
      shinydashboard::tabItem(
        tabName = "activity",
        fluidPage(
          table_ui(id = "check_fishing_context", title = "Fishing context", text = "<ul><li>If the school type is \"object school\" (code 1), then there must be at least one object-type association</li>
                 <li>If the school type is \"free school\" (code 2), then the association identifier, if it exists, must not be of object type</li></ul>"),
          table_ui(id = "check_operationt", title = "Operation", text = "<ul><li>If the vessel activity is \"Fishing (skiff is deployed)\" (code 6), then it needs a success status, otherwise it must not have any</li>
                 <li>If the school type is \"Undefined\" (code 0), success type must not be \"Positive\" (code 1) or \"Null\" (code 0)</li>
                 <li>If the weight is not 0 then success type must be \"Positive\" (code 1) or \"Unknown\" (code 2), otherwise, if it exists, must be 0</li></ul>"),
          table_ui(id = "check_position", title = "Position", text = "<ul><li>If the position is on land, you need to check the latitude and longitude</li>
                 <li>If the position and ocean are different, you need to check these fields with the logbook</li></ul>"),
          table_ui(id = "check_weight", title = "Total Catch Weight", text = "<ul><li>If the values are different, you must transfer the sum of the elementary captures to the activity</li></ul>"),
          table_ui(id = "check_temperature", title = "Temperature", text = "<ul><li>Indicates whether the temperature is between 15 and 32 for the activity</li></ul>"),
          table_ui(id = "check_weighting_sample", title = "Weighting sample", text = "<ul><li>Indicates whether the sum of the catch weights for the activity is consistent with the sum of the sample weighted weights link for the activity</li></ul>"),
          table_ui(id = "check_time_route", title = "Time route", text = "<ul><li>Indicates whether sea time and fishing time exceed their maximum limits (default 24 for sea time and 13 for fishing time).</li>
                   <li>Indicates whether sea time is less than fishing time</li>
                   <li>Indicates whether sea time equals 0 while sea activities are associated</li>
                   <li>Indicates whether fishing time equals 0 while fishing activities are associated</li></ul>"),
          table_ui(id = "check_eez", title = "EEZ", text = "<ul><li>Indicates when the declared and calculated eez are different</li>
                   <li>Indicates when there is no declared eez for fishing activities </li></ul>")
          )
      ),
      shinydashboard::tabItem(
        tabName = "sample",
        fluidPage(
          table_ui(id = "check_length_class", title = "Size class", text = "<ul><li>Indicates FL measurements of species 'YFT', 'BET', 'ALB' greater than 80</li></ul>"),
          table_ui(id = "check_measure", title = "Measurement", text = "<ul><li>Indicates per sample whether the sum of individuals measured in the species samples is different from the sum of individuals measured in the size classes</li></ul>"),
          table_ui(id = "check_species", title = "Species", text = "<ul><li>Indicates species not conforming with respect to the following list:'YFT', 'SKJ', 'BET', 'ALB', 'LTA', 'FRI', 'TUN', 'KAW', 'LOT'</li></ul>"),
          table_ui(id = "check_sample_without_measure", title = "Sample without measurement", text = "<ul><li>Indicates species samples that have no size measurement</li></ul>"),
          table_ui(id = "check_sample_without_species", title = "Sample without species", text = "<ul><li>Indicates samples that have no species samples</li></ul>"),
          table_ui(id = "check_super_sample_number", title = "Super sample", text = "<ul><li>If the sample is a super sample, then there must be several sub-samples, numbered from 1</li>
                   <li>Otherwise there must be a single sub-sample, numbered 0</li></ul>"),
          table_ui(id = "check_well_number", title = "Well", text = "<ul><li>The sample well must exist in the trip well plan</li></ul>"),
          table_ui(id = "check_little_big", title = "Ratio of small over big fish", text = "<ul><li>The percentages of small over big fish must be consistent within the sample</li></ul>"),
          table_ui(id = "check_weighting", title = "Weighting", text = "<ul><li>The weighting for each sample must be coherent</li></ul>"),
          table_ui(id = "check_weight_sample", title = "Weight sample", text = "<ul><li>Only one value must be entered between the total weight and the count of the weight of small or big fish</li></ul>"),
          table_ui(id = "check_activity_sample", title = "Sample activity", text = "<ul><li>Sample must be linked to an activity</li></ul>"),
          table_ui(id = "check_ldlf", title = "LDLF", text = "<ul><li>The type of measure must be compatible with species</li>
                 <li>The type of measurement must be compatible with total, small and big fish weights</li></ul>"),
          table_ui(id = "check_distribution", title = "Distribution +10/-10", text = "<ul><li>Sample must have -10/+10 distribution values consistent with those of the reported well</li></ul>"),
          table_ui(id = "check_sample_harbour", title = "Sample harbour", text = "<ul><li>Samples must have a harbour of landing</li></ul>")
        )
      ),
      shinydashboard::tabItem(
        tabName = "anapo",
        fluidPage(
          table_ui(
            id = "check_anapo", title = "Anapo", text = "<ul><li>There must be at least 20 VMS positions during the day</li>
                 <li>There must be at least one VMS position nearer than 10 miles away OR the score (resulting from geographical and temporal distance) must be greater than or equal to 0.5 OR the position must be in a harbour</li></ul>",
            size_box = "col-sm-12 col-md-12 col-lg-12"
          ),
          table_ui(
            id = "check_anapo_activity", title = "Anapo activity", text = "<ul><li>Each VMS must have at least one existing activity, for vessel types seiner (large and without bait (5,6)), bait boat (freezer and ice (1,2)) and supply (10)</li></ul>
            (Warning: in case of inconsistency all vessels (Vessel code) linked to the VMS vessel code are displayed inconsistently, select only active vessels (Vessel status 1) if you are not working on historical data)",
            size_box = "col-sm-12 col-md-12 col-lg-12")
        )
      ),
      shinydashboard::tabItem(
        tabName = "summary",
        fluidPage(
          textOutput("text_summary")
        )
      ),
      shinydashboard::tabItem(
        tabName = "setting",
        fluidPage(
          fileInput(
            inputId = "setting_file_path", label = paste0("Path of the configuration file .yml ; (Default: ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"), ")"),
            multiple = FALSE,
            accept = c(".yml")
          )
        )
      )
    )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AkadoR"
    )
  )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
}
