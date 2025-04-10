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
          tags$style(
            HTML("#sidebarItemExpanded > ul > :last-child {
                          position: absolute;
                          bottom: 0;
                          width: 100%;}"
            )
          )
        ),
        # Fix the scrollbar with withSpinner effect
        tags$head(
          tags$style(
            HTML(".wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}")
          )
        ),
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
          padding-right:10px;}")),
        # Change position of sort icon in display tables
        tags$style(HTML("table.dataTable thead>tr>th.sorting:before,
          table.dataTable thead>tr>th.sorting:after{
          right: 0px}")),
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
              # Display of check group set created by function table_group_server
              uiOutput(NS(namespace = "trip", id = "multiple_table"))
            )
          ),
          shinydashboard::tabItem(
            tabName = "activity",
            fluidPage(
              # Display of check group set created by function table_group_server
              uiOutput(NS(namespace = "activity", id = "multiple_table"))
            )
          ),
          shinydashboard::tabItem(
            tabName = "sample",
            fluidPage(
              # Display of check group set created by function table_group_server
              uiOutput(NS(namespace = "sample", id = "multiple_table"))
            )
          ),
          shinydashboard::tabItem(
            tabName = "anapo",
            fluidPage(
              # Display of check group set created by function table_group_server
              uiOutput(NS(namespace = "anapo", id = "multiple_table"))
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
