# Function that tests the presence of activity associated with the trip, in the future integrated in the pakage codama
check_trip_activityinspector <- function(data_connection,
                                         select,
                                         output) {
  # 0 - Global variables assignement ----
  first_vector <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "message"
    ))
  } else {
    if (r_type_checking(
      r_object = data_connection[[2]],
      type = "PostgreSQLConnection",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = data_connection[[2]],
        type = "PostgreSQLConnection",
        output = "message"
      ))
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_values = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_values = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type of select
    if (r_type_checking(
      r_object = select,
      type = "character",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "character",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Trip associated with route and activity
    trip_with_activity_sql <- paste(
      readLines("./sql/trip_with_activity.sql"),
      collapse = "\n"
    )
    trip_with_activity_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = trip_with_activity_sql,
      select_item = DBI::SQL(paste(paste0("'", select, "'"),
                                   collapse = ", "
      ))
    )
    trip_with_activity_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = trip_with_activity_sql
    ))
  } else {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Consistency check not developed yet for this \"data_connection\" argument.\n ",
      sep = ""
    )
  }
  # 3 - Data design ----
  nrow_first <- length(select)
  # Search trip ID in the associations trip, route, activity
  comparison <- vectors_comparisons(select,
                                    trip_with_activity_data$trip_id,
                                    comparison_type = "difference",
                                    output = "report"
  )
  comparison$logical <- FALSE
  colnames_comparison <- grep("vectors_comparisons_", colnames(comparison))
  comparison$logical[comparison[, colnames_comparison] == "no difference"] <- TRUE
  select <- cbind(select, comparison)
  if ((sum(select$logical) + sum(!select$logical)) != nrow_first) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      sep = ""
    )
  }
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!select$logical), " trip with no route")))
  }
  if (output == "report") {
    select <- select[,-(colnames_comparison+1)]
    select <- subset(select, select = -c(first_vector))
    return(select)
  }
  if (output == "logical") {
    return(select$logical)
  }
}
