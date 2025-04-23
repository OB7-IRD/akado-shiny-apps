#' @title table UI Function
#' @description A shiny Module for creation of all table
#' @param id Internal parameters for shiny
#' @param title {\link[base]{character}} expected. Default values: NULL. Name to be displayed for check
#' @param size_box {\link[base]{character}} expected. Default values: "col-sm-12 col-md-6 col-lg-6". Check size specification for each window size type (format "col-sm-your_size col-md-your_size col-lg-your_size")
#' @param text {\link[base]{character}} expected. Default values: NULL. Text to be displayed for check
#' @return The function returns a {\link[shiny]{tagList}} with a {\link[shinydashboard]{box}} and {\link[DT]{DTOutput}}
#' @export
mod_table_ui <- function(id, title = NULL, size_box = "col-sm-12 col-md-6 col-lg-6", text = NULL) {
  ns <- shiny::NS(id)
  tagList(
    div(
      id = paste0("div_", id),
      class = size_box,
      shinydashboard::box(
        width = "100%",
        title = title,
        status = "primary",
        solidHeader = TRUE,
        HTML(text),
        shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, size = 0.5, proxy.height = "70px")
      )
    )
  )
}

#' @title table Server Functions
#' @description A shiny Module for creation of all table
#' @param id Internal parameters for shiny
#' @param data {\link[base]{list}} expected. Reactive list containing results tables for all checks
#' @param name {\link[base]{character}} expected. Control identifier in data to be displayed
#' @param type_line_check {\link[base]{character}} expected. Reactive value containing the type of line selected by the user
#' @param column_no_wrap {\link[base]{integer}} expected. Default values: NULL. Column numbers that should not be subject to automatic line breaks
#' @return The function returns nothing, instantiating the table
#' @export
mod_table_server <- function(id, data, name, type_line_check, column_no_wrap = NULL) {
  # Local binding global variables
  . <- NULL
  # If no name is specified, use id as name
  if (missing(name)) {
    name <- id
  }
  moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (isTruthy(data()) && isTruthy(type_line_check())) {
        data <- data()[[name]]
        if (type_line_check() == "inconsistent") {
          data <- data[data$Check != as.character(icon("check")), ]
        }
        data <- DT::datatable(data,
                              escape = FALSE,
                              rownames = FALSE,
                              extensions = "Buttons",
                              options = list(lengthChange = FALSE,
                                             scrollX = TRUE,
                                             dom = "Bfrtip",
                                             buttons = list(list(extend = "copy",
                                                                 text = "Copy data displayed"),
                                                            list(extend = "collection",
                                                                 text = "Download all data",
                                                                 action = DT::JS(paste0("function(){Shiny.setInputValue('button_download', '", name, "', {priority: 'event'});}")))))) %>%
          # If data is not empty
          {if (ncol(data) > 0) DT::formatStyle(., columns = column_no_wrap, "white-space" = "nowrap") else .
          }
        return(data)
      }
    })
  })
}

## To be copied in the UI
# mod_table_ui("table_1", title, size_box, text)

## To be copied in the server
# mod_table_server("table_1", data, name, type_line_check, column_no_wrap)
