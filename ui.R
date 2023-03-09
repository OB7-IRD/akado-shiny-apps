# package
packages <- c("shinydashboard", "shinyWidgets")
install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
lapply(packages, library, character.only = TRUE)



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
            inputId = "vessel_number", label = "Indicate a vessel number:", value = 0, min = 0
          )),
          column(width = 6, dateInput(
            "trip_end_date", "Choose a trip end date:"
          ))
        ),
        h2("Multiple selection by date range"),
        fluidRow(
          column(width = 6, dateInput(
            inputId = "trip_start_date_range", label = "Choose a trip start date:"
          )),
          column(width = 6, dateInput(
            "trip_end_date_range", "Choose a trip end date:"
          ))
        ),
        span(textOutput("error_date_select"), style = "color:red;")
      ),
      box(
        width = 12,
        align="center",
      actionButton("start_button", "Start"))
    )
  ),
  tabItem(
    tabName = "trip",
    fluidPage()
  )
)


ui <- dashboardPage(header, siderbar, body)
