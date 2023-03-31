# package
packages <- c("shinydashboard", "shinyWidgets", "furdeb", "DBI", "dplyr", "codama", "shinycssloaders", "DT")
install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
source("./function.R")


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
      menuItem("Setting", tabName = "setting", icon = icon("gear"))
    )
  )

body <- dashboardBody(
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
  # Color of the icons in relation to the consistency test
  tags$style(".fa-check {color:#05DE1E}"),
  tags$style(".fa-xmark {color:#DE0505}"),
  # Green background color for notifications id = "notif_default"
  tags$style("#shiny-notification-notif_default {background-color:#B2E8A2;}"),
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
          style = "padding-bottom:55px;",
          align = "center",
          actionButton(inputId = "start_button", label = "Start"),
          withSpinner(htmlOutput("error_trip_select"), type = 6, size = 0.5, proxy.height = "70px")
        )
      )
    ),
    tabItem(
      tabName = "trip",
      fluidPage(
        box(
          width = 5,
          h1("Presence of activity"),
          withSpinner(DTOutput("check_trip_activity"), type = 1)
        ),
        box(
          width = 6,
          h1("Fishing time"),
          withSpinner(DTOutput("check_fishing_time"), type = 1)
        )
      )
    ),
    tabItem(
      tabName = "setting",
      fluidPage(
        fileInput(
          inputId = "setting_file_path", label = paste0("Path of the configuration file .yml ; (Default: ", path.expand("~"), "/.appconfig/configuration_file.yml)"),
          multiple = FALSE,
          accept = c(".yml")
        )
      )
    )
  )
)

ui <- dashboardPage(header, siderbar, body)
