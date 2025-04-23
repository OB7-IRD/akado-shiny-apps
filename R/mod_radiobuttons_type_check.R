#' @title radiobuttons_type_check UI Function
#' @description A shiny Module for creation of radio button to select the display of controls
#' @param id Internal parameters for shiny
#' @return The function returns a {\link[shiny]{tagList}} with a {\link[shiny]{radioButtons}}
#' @export
mod_radiobuttons_type_check_ui <- function(id) {
  ns <- shiny::NS(namespace = id)
  shiny::tagList(
    uiOutput(ns("radiobuttons_type_check")),
    # Font size buttons in menu
    tags$style(paste0("#", id, "-radiobuttons_type_check {font-size:11px;}"))
  )
}

#' #' @title radiobuttons_type_check Server Functions
#' @description A shiny Module for creation of all tab
#' @param id Internal parameters for shiny
#' @param type_check_info {\link[base]{list}} expected. Information about the dynamic type of check
#' @return The function returns nothing, instantiating the radiobuttons
#' @details
#' \itemize{
#' type_check_info:
#' \item{\code{  title : Button title, mandatory, character expected}}
#' \item{\code{  id : Button selection identification, mandatory, character expected}}
#' \item{\code{  text : Display text for button selection, mandatory, character expected}}
#' }
#' @export
mod_radiobuttons_type_check_server <- function(id, type_check_info) {
  moduleServer(id, function(input, output, session) {
    output$radiobuttons_type_check <- shiny::renderUI({
      id_choice <- lapply(type_check_info, function(check) {
        if (is.list(check)) {
          check[["id"]]
        }
      })
      text_choice <- lapply(type_check_info, function(check) {
        if (is.list(check)) {
          check[["text"]]
        }
      })
      radioButtons(inputId = shiny::NS(namespace = id, id = "type_check"), label = type_check_info[["title"]], choiceNames = unlist(text_choice), choiceValues = unlist(id_choice))
    })
  })
}

## To be copied in the UI
# mod_radiobuttons_type_check_ui("radiobuttons_type_check_1")

## To be copied in the server
# mod_radiobuttons_type_check_server("radiobuttons_type_check_1", type_check_info)
