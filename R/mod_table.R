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
#' @param referential_file {\link[base]{list}} expected. Reactive list containing referential tables for all plot
#' @param column_no_wrap {\link[base]{integer}} expected. Default values: NULL. Column numbers that should not be subject to automatic line breaks
#' @param function_plot {\link[base]{character}} expected. Default values: NULL. Name of the function that creates the plot
#' @param function_text_plot {\link[base]{character}} expected. Default values: NULL. Name of the function that creates the text to be displayed in the plot window
#' @param title_window {\link[base]{character}} expected. Default values: NULL. Plot window name
#' @return The function returns nothing, instantiating the table
#' @export
mod_table_server <- function(id, data, name, type_line_check, referential_file, column_no_wrap = NULL, function_plot = NULL, function_text_plot = NULL, title_window = NULL) {
  # Local binding global variables
  . <- NULL
  # If no name is specified, use id as name
  if (missing(name)) {
    name <- id
  }
  moduleServer(id, function(input, output, session) {
    # Display table
    output$table <- DT::renderDT({
      # If there was no error in the trip selection and that there are trips for user settings and the calculations for the consistency tests are finished, displays the table
      if (isTruthy(data()) && isTruthy(type_line_check())) {
        # In the event of an error during control, the error is displayed instead of the table
        if (!is.null(data()[[name]][["error"]])) {
          stop(paste("Error, check failure: ", data()[[name]][["error"]]))
        }
        data <- data()[[name]][["table"]]
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

    # Display window and plot
    if (!is.null(function_plot)) {
      # Button name
      name_button <- paste0("button_", id)
      # Control plot, display in a window
      output$plot <- plotly::renderPlotly({
        split_id <- strsplit(input[[name_button]], "&")[[1]]
        data <- data()[[split_id[1]]][["list_plot"]][[split_id[2]]]
        # Retrieves the reference files indicated by data, which will then be used by plot
        list_tmp_referential_file <- list()
        data_tmp <- data
        for (i in seq_along(data)) {
          if (length(data[[i]]) == 1 && data[[i]] %in% names(referential_file())) {
            list_tmp_referential_file[[names(data[i])]] <- referential_file()[[data[[i]]]]
            data_tmp[[names(data[i])]] <- NULL
          }
        }
        # Retrieves all values of plot arguments
        do.call(function_plot, c(data_tmp[names(data_tmp) %in% names(formals(function_plot))], list_tmp_referential_file[names(list_tmp_referential_file) %in% names(formals(function_plot))]))
      })

      # Control window
      observeEvent(input[[name_button]], {
        split_id <- strsplit(input[[name_button]], "&")[[1]]
        data <- data()[[split_id[1]]][["list_plot"]][[split_id[2]]]
        # Executes, if supplied by user, the function that indicates the text to be displayed
        if (!is.null(function_text_plot)) {
          text <- do.call(function_text_plot, data[names(data) %in% names(formals(function_text_plot))])
        } else {
          text <- ""
        }
        # Recover window title, if supplied by user
        if (!is.null(title_window)) {
          title <- title_window
        } else {
          title <- ""
        }
        # Display window
        showModal(modalDialog(
          fluidRow(
            column(3,
                   style = "padding-left:5px;padding-right:0px;",
                   HTML(
                     # Non-breaking hyphen (-)
                     gsub("-", "&#8209;", text)
                   )),
            column(9,
                   style = "padding-left:0px;padding-right:5px;",
                   plotly::plotlyOutput(shiny::NS(namespace = id, "plot")))
          ),
          title = title,
          size = "l",
          fade = TRUE,
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
}

## To be copied in the UI
# mod_table_ui("table_1", title, size_box, text)

## To be copied in the server
# mod_table_server("table_1", data, name, type_line_check, column_no_wrap, function_plot, function_text_plot, title_plot)
