#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui,
        enable_admin = TRUE,
        head_auth = golem_add_external_resources(),
        tags_top =
          tags$div(
            tags$head(
              # Format text "language" and select language
              tags$style(
                "#auth-label_language {
                  width: max-content;
                  margin: auto;
                  margin-right: 0px;}
                #auth-language-selectized {
                  display: none !important;}"
              )
            ),
            br(),
            # Title, Logo
            tags$h4("AKaDoR", style = "align:center"),
            tags$img(
              src = file.path("www", "favicon.png"),
              width = 100
            )
          ),
        # Language
        choose_language = c("en", "es", "fr"),
        language = "en",
        # Time out 1h
        timeout= 60
      ),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
