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
          id = "sidebarmenu_id",
          style = "overflow: visible",
          mod_tab_menu_ui("tab")
        )
      ),
      shinydashboard::dashboardBody(
        # Allows you to hide or show the boxes according to
        shinyjs::useShinyjs(),
        # Fix the setting onget at the bottom
        tags$head(
          tags$style(
            HTML("#tab-menu > ul > :last-child {
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
        mod_tab_content_ui("tab")
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
