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
#' @param tab_info {\link[base]{list}} expected. Information about the dynamic tab display
#' @param check_info {\link[base]{list}} expected. Information about the dynamic check display
#' @return The function returns nothing, instantiating the tab
#' @details
#' \itemize{
#' tab_info:
#'  \item{\code{  id : Tab identification, mandatory, character expected}}
#' }
#' \itemize{
#' check_info:
#'  \item{\code{  id : Check identification, mandatory, character expected}}
#'  \item{\code{  tab : Tab identification, mandatory, character expected}}
#'  \item{\code{  title : Name to be displayed for check, optional, character expected}}
#'  \item{\code{  text : Text to be displayed for check, optional, character expected}}
#'  \item{\code{  column_no_wrap : Column numbers that should not be subject to automatic line breaks, optional, integer expected}}
#'  \item{\code{  size_box : Check size specification for each window size type (format "col-sm-your_size col-md-your_size col-lg-your_size"), optional, character expected}}
#' }
#' @export
mod_tab_content_server <- function(id, tab_info, check_info) {
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
        )),
        # Creating dynamic tab
        # Use of lapply and not a for loop to avoid lazy evaluation problems
        lapply(
          tab_info,
          function(tab) {
            shinydashboard::tabItem(
              tabName = tab[["id"]],
              fluidPage(
                lapply(
                  check_info,
                  function(check) {
                    if (check[["tab"]] == tab[["id"]]) {
                      # Configure display check with recovered parameters
                      do.call(table_ui, check[names(check) %in% c("id", "title", "text", "size_box")])
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
            fileInput(
              inputId = "setting_file_path", label = paste0("Path of the configuration file .yml ; (Default: ", file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"), ")"),
              multiple = FALSE,
              accept = c(".yml")
            )
          )
        ))
      )
      do.call(shinydashboard::tabItems, items)
    })
  })
}

## To be copied in the UI
# mod_tab_content_ui("tab_content_1")

## To be copied in the server
# mod_tab_content_server("tab_content_1", tab_info, check_info)
