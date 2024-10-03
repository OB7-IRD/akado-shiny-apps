#' @name set_start_configuration
#' @title AkadoR set startup configuration
#' @description The purpose of the set_start_configuration function allows application launch parameters to be modified by an external file
#' @param path_file_configuration {\link[base]{character}} expected. Default values: file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"). Path to file containing configuration parameters, default value adapts to different operating systems
#' @export
set_start_configuration <- function(path_file_configuration = file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")) {
  # 1 - Arguments verification ----
  # Checks the type of path_file_configuration
  if (!codama::r_type_checking(
    r_object = path_file_configuration,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = path_file_configuration,
      type = "character",
      length = 1L,
      output = "message"
    ))
  }
  # 2 - main ----
  if (file.exists(path_file_configuration)) {
    # Read configuration file
    file_configuration <- furdeb::configuration_file(
      path_file = path_file_configuration,
      silent = TRUE
    )
    # Initialize variables to configure application launch
    names_argument <- c()
    value_argument <- list()
    if (!is.null(file_configuration[["start_AkadoR_configuration"]][["secure_connection"]])) {
      names_argument <- c(names_argument, "secure_connection")
      value_argument <- append(value_argument, file_configuration[["start_AkadoR_configuration"]][["secure_connection"]])
    }
    # Configure application launch with recovered parameters
    call_start_configuration <- do.call("start_configuration", stats::setNames(value_argument, names_argument))
  } else {
    message("No application startup configuration file found")
    call_start_configuration <- start_configuration()
  }
  return(call_start_configuration)
}

#' @name start_configuration
#' @title AkadoR startup configuration
#' @description The purpose of the start_configuration function is to configure startup, secure connection to the application, ...
#' @param secure_connection {\link[base]{logical}} expected. Default values: FALSE. If you wish to enable secure connection to the application, set TRUE
#' @export
start_configuration <- function(secure_connection = FALSE) {
  # 1 - Arguments verification ----
  # Checks the type of secure_connection
  if (!codama::r_type_checking(
    r_object = secure_connection,
    type = "logical",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = secure_connection,
      type = "logical",
      length = 1L,
      output = "message"
    ))
  }
  # 2 - main ----
  # Secure connection to the application
  if (secure_connection == TRUE) {
    call_ui <- shinymanager::secure_app(app_ui,
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
      language = "en"
    )
  } else {
    call_ui <- app_ui
  }
  return(call_ui)
}

#' @name set_server_authentication
#' @title AkadoR set server authentication
#' @description The purpose of the set_server_authentication function allows application launch parameters to be modified by an external file
#' @param path_file_configuration {\link[base]{character}} expected. Default values: file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml"). Path to file containing configuration parameters, default value adapts to different operating systems
#' @export
set_server_authentication <- function(path_file_configuration = file.path(path.expand("~"), ".appconfig", "akador", "configuration_file.yml")) {
  # 1 - Arguments verification ----
  # Checks the type of path_file_configuration
  if (!codama::r_type_checking(
    r_object = path_file_configuration,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = path_file_configuration,
      type = "character",
      length = 1L,
      output = "message"
    ))
  }
  # 2 - main ----
  if (file.exists(path_file_configuration)) {
    # Read configuration file
    file_configuration <- furdeb::configuration_file(
      path_file = path_file_configuration,
      silent = TRUE
    )
    # Initialize variables to configure authentication
    names_argument <- c()
    value_argument <- list()
    if (!is.null(file_configuration[["start_AkadoR_configuration"]][["path_database"]])) {
      names_argument <- c(names_argument, "path_database")
      value_argument <- append(value_argument, file_configuration[["start_AkadoR_configuration"]][["path_database"]])
    }
    if (!is.null(file_configuration[["start_AkadoR_configuration"]][["set_timeout"]])) {
      names_argument <- c(names_argument, "set_timeout")
      value_argument <- append(value_argument, file_configuration[["start_AkadoR_configuration"]][["set_timeout"]])
    }
    # Configure server authentication with recovered parameters
    call_server_authentication <- do.call("server_authentication", stats::setNames(value_argument, names_argument))
  } else {
    message("No application startup configuration file found")
    call_server_authentication <- server_authentication()
  }
  return(call_server_authentication)
}

#' @name server_authentication
#' @title Authentication result for secure connection to AkadoR
#' @description The purpose of the server_authentication function enables connection authentication
#' @param path_database {\link[base]{character}} expected. Default values: file.path(path.expand("~"), ".appconfig", "akador", "database_connection.sqlite"). Path to file containing data base connection, default value adapts to different operating systems
#' @param set_timeout {\link[base]{numeric}} expected. Default values: 60. Session expiry time (in minutes) before disconnection in standby mode if secure connection is enabled
#' @export
server_authentication <- function(path_database = file.path(path.expand("~"), ".appconfig", "akador", "database_connection.sqlite"), set_timeout = 60) {
  # 1 - Arguments verification ----
  # Checks the type of path_database
  if (!codama::r_type_checking(
    r_object = path_database,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = path_database,
      type = "character",
      length = 1L,
      output = "message"
    ))
  }
  # Checks the type of set_timeout
  if (!codama::r_type_checking(
    r_object = set_timeout,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = set_timeout,
      type = "numeric",
      length = 1L,
      output = "message"
    ))
  }
  # 2 - main ----
  if (!file.exists(path_database)) {
    # Creation of a database by default, administrator must then modify password and user name
    database_connection <- data.frame(
      user = c("shiny", "shinymanager"),
      password = c("azerty", "12345"),
      admin = c(FALSE, TRUE)
    )
    # Path verification
    if (file.exists(gsub(paste(basename(path_database), "$", sep = ""), "", path_database))) {
      shinymanager::create_db(
        credentials_data = database_connection,
        sqlite_path = path_database
      )
    }else {
      warning(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " cannot write data base ' ",
        path_database, "' : No such directory",
        sep = ""
      )
    }
  }
 # Authentication result
  call_check_credentials <- shinymanager::check_credentials(path_database)
# Server Authentication
  call_secure_server <- shinymanager::secure_server(
    check_credentials = call_check_credentials,
    timeout = set_timeout
  )
  return(call_secure_server)
}
