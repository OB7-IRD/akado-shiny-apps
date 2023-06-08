# package
packages <- c("shinydashboard", "shinyWidgets", "furdeb", "DBI", "dplyr", "shinycssloaders", "DT", "shinyjs", "plotly")
install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
if (length(setdiff("codama", rownames(installed.packages()))) > 0) {
  devtools::install_github("https://github.com/OB7-IRD/codama", ref = "development", INSTALL_opts = c("--no-multiarch"))
}
packages <- c(packages, "codama")
lapply(packages, library, character.only = TRUE)
source(file.path(".", "function.R"))

header <- dashboardHeader(
  title = "AKaDo",
  disable = FALSE,
  titleWidth = 550
)
# Add logo
header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(tags$img(src = "akado.png", height = "100%", align = "left"),
                                             target = "_blank"
)

siderbar <-
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      style = "overflow: visible",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Trip", tabName = "trip", icon = icon("ship")),
      menuItem("Activity", tabName = "activity", icon = icon("list-check")),
      menuItem("Sample", tabName = "sample", icon = icon("fish")),
      menuItem("Well", tabName = "well", icon = icon("boxes-stacked")),
      menuItem("Anapo", tabName = "anapo", icon = icon("route")),
      menuItem("Summary", tabName = "summary", icon = icon("scroll")),
      hr(style= "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #333, #ccc, #333); background-image: -moz-linear-gradient(left, #333, #ccc, #333); background-image: -ms-linear-gradient(left,#333, #ccc, #333); background-image: -o-linear-gradient(left, #333, #ccc, #333);"),
      radioButtons(inputId = "type_check_trip", label = "Choice the display of verification types", choiceNames = c("All (Info, warning or error)", "Info only", "Warning only", "Error only"), choiceValues = list("All", "Info", "Warning", "Error")),
      radioButtons(inputId = "type_line_check_trip", label = "Choice of line type display", choiceNames = list(HTML(paste0("All ( ", icon("check"), " - ", icon("info"), " - ", icon("exclamation"), " - ", icon("xmark"), " )")), HTML(paste0("Info, warning or error ( ", icon("info"), " - ", icon("exclamation"),  " - ", icon("xmark"), " )"))), choiceValues = list("All", "inconsistent")),
      
      menuItem("Setting", tabName = "setting", icon = icon("gear"))
    )
  )

body <- dashboardBody(
  # Allows you to hide or show the boxes according to
  useShinyjs(),
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
  tags$style("#type_check_trip, #type_line_check_trip {font-size:11px;}"),
  # Add pages
  tabItems(
    tabItem(
      tabName = "home",
      fluidPage(
        box(
          width = 12,
          h1("Contexte"),
          p("AKADO automatically performs a series of tests on the data and produces summary tables that provide a more or less detailed assessment of the anomalies detected. A final summary presents the percentages of occurrences of errors that remain to be corrected.")
        ),
        box(
          width = 12,
          h1("Data base Observe"),
          selectInput(
            inputId = "data_base_observe", label = "Choose a data base:",
            choices = list("---", "NY", "NJ", "CT")
          )
        ),
        box(
          width = 12,
          h1("Selection of the trip "),
          h2("Unique selection by vessel number and trip end date"),
          fluidRow(
            column(width = 6, numericInput(
              inputId = "vessel_number", label = "Indicate a vessel number:", value = NA, min = 0
            )),
            column(width = 6, dateInput(
              inputId = "trip_end_date", label = "Choose a trip end date:", value = NA
            ))
          ),
          h2("Multiple selection by date range"),
          fluidRow(
            column(width = 6, dateInput(
              inputId = "trip_start_date_range", label = "Choose a trip start date:", value = NA
            )),
            column(width = 6, dateInput(
              inputId = "trip_end_date_range", label = "Choose a trip end date:", value = NA
            ))
          ),
          span(textOutput("error_date_select"), style = "color:red;")
        ),
        box(
          width = 12,
          align = "center",
          actionButton(inputId = "start_button", label = "Start"),
          withSpinner(htmlOutput("error_trip_select"), type = 6, size = 0.5, proxy.height = "70px")
        )
      )
    ),
    tabItem(
      tabName = "trip",
      fluidPage(
        div(
          id = "div_check_trip_activity",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Presence of activity"),
            withSpinner(DTOutput("check_trip_activity"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_fishing_time",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Fishing time"),
            HTML("<ul><li>If the values are not equivalent, you must enter the sum in the 'Fishing Time' field of the tide</li></ul>"),
            withSpinner(DTOutput("check_fishing_time"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_sea_time",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Sea time"),
            HTML("<ul><li>If the values are not equivalent, you must enter the sum in the 'Sea Time' field of the tide</li></ul>"),
            withSpinner(DTOutput("check_sea_time"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_landing_consistent",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Vessel capacity"),
            HTML("<ul><li>If the total landed weight is greater than the vessel's capacity, you must verify that the 'landed weight' is correct</li></ul>"),
            withSpinner(DTOutput("check_landing_consistent"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_landing_total_weigh",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Total landed weight"),
            HTML("<ul><li>If the values are not equal, you must enter the value of the sum of the commercial lots in the 'Landed Weight' field of the trip</li></ul>"),
            withSpinner(DTOutput("check_landing_total_weigh"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_temporal_limit",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Time coverage"),
            HTML("<ul><li>You must check the fishing log to see if a day is missing</li>
                 <li>You must verify that the departure and arrival dates match in the fishing logbook and landing documents</li></ul>"),
            withSpinner(DTOutput("check_temporal_limit"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_harbour",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Harbour"),
            HTML("<ul><li>Check if all fishing logs have been entered.</li>
                 <li>Check with the captain to see if any outings have been made in the meantime.</li></ul>"),
            withSpinner(DTOutput("check_harbour"), type = 6, size = 0.5, proxy.height = "70px")
          )
        ),
        div(
          id = "div_check_raising_factor",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Raising Factor"),
            HTML("<ul><li>If the ratio is not between 0.9 < R < 1.1 (Landing/catch), you need to check the partial landing value.</li></ul>"),
            withSpinner(DTOutput("check_raising_factor"), type = 6, size = 0.5, proxy.height = "70px")
          )
        )
      )
    ),
    tabItem(
      tabName = "activity",
      fluidPage(
        div(
          id = "div_check_fishing_context",
          class = "col-sm-12 col-md-6 col-lg-4",
          box(
            width = "100%",
            h1("Fishing context"),
            HTML("<ul><li>If the school type is \"object school\" (code 1), then there must be at least one object-type association.</li>
                 <li>If the school type is \"free school\" (code 2), then the association identifier, if it exists, must not be of object type</li></ul>"),
            withSpinner(DTOutput("check_fishing_context"), type = 6, size = 0.5, proxy.height = "70px")
          )
        )
      )
    ),
    tabItem(
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

ui <- dashboardPage(header, siderbar, body)
