#' @title tab_menu UI Function
#' @description A shiny Module for creation of all menus
#' @param id Internal parameters for shiny
#' @return The function returns a {\link[shiny]{tagList}} with a {\link[shinydashboard]{sidebarMenu}} and {\link[shinydashboard]{menuItem}}
#' @export
mod_tab_menu_ui <- function(id) {
  ns <- shiny::NS(namespace = id)
  shiny::tagList(
    uiOutput(ns("menu")),
    # Font size buttons in menu
    tags$style(paste0("#", id, "-type_line_check {font-size:11px;}"))
  )
}

#' @title tab_menu Server Functions
#' @description A shiny Module for creation of all menus
#' @param id Internal parameters for shiny
#' @param tab_info {\link[base]{list}} expected. Information about the dynamic tab display
#' @return The function returns a reactive valu
#' @details
#' \itemize{
#' tab_info:
#'  \item{\code{  id : Tab identification, mandatory, character expected}}
#'  \item{\code{  title : Name to be displayed for tab, mandatory, character expected}}
#'  \item{\code{  icon : Picture to be displayed for tab, optional, character expected}}
#' }
#' @export
mod_tab_menu_server <- function(id, tab_info) {
  moduleServer(id, function(input, output, session) {
    # Creation of all menus, both static tab menus and user-specified dynamic menus (statics are also created here to avoid display problems)
    output$menu <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(
        # Creating the static home menu
        shinydashboard::menuItem("Home", tabName = "home", icon = icon("house")),
        # Creating dynamic menu
        # Use of lapply and not a for loop to avoid lazy evaluation problems
        lapply(
          tab_info,
          function(tab) {
            tab_tmp <- list(tabName = tab[["id"]], text = tab[["title"]])
            if ("icon" %in% names(tab)) {
              tab_tmp <- c(tab_tmp, icon = list(icon(tab[["icon"]])))
            }
            # Configure display tab menu with recovered parameters
            do.call(shinydashboard::menuItem, tab_tmp[names(tab_tmp) %in% c("tabName", "text", "icon")])
          }
        ),
        # Creating the static summary menu
        shinydashboard::menuItem("Summary", tabName = "summary", icon = icon("scroll")),
        # Creating static radioButtons
        hr(style = "border: 0;height: 1px; background-image: -webkit-linear-gradient(left, #333, #ccc, #333); background-image: -moz-linear-gradient(left, #333, #ccc, #333); background-image: -ms-linear-gradient(left,#333, #ccc, #333); background-image: -o-linear-gradient(left, #333, #ccc, #333);"),
        mod_radiobuttons_type_check_ui(id),
        radioButtons(inputId = shiny::NS(namespace = id, id = "type_line_check"), label = "Choose the display of line type", choiceNames = list(HTML(paste0("All ( ", icon("check"), " - ", icon("info"), " - ", icon("exclamation"), " - ", icon("xmark"), " )")), HTML(paste0("Info, warning or error ( ", icon("info"), " - ", icon("exclamation"), " - ", icon("xmark"), " )"))), choiceValues = list("All", "inconsistent")),
        # Creating the static setting menu
        shinydashboard::menuItem("Settings", tabName = "setting", icon = icon("gear"))
      )
    })
    # Return reactive value use by other module
    return(
      list(
        type_line_check = reactive(input$type_line_check)
      )
    )
  })
}

## To be copied in the UI
# mod_tab_menu_ui("name_of_module1_1")

## To be copied in the server
# mod_tab_menu_server("name_of_module1_1", tab_info)
