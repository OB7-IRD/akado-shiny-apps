

shinyServer(function(input, output, session) {
  # Error message if the date range is not correct
  output$error_date_select <- renderText({
    if (input$trip_start_date_range > input$trip_end_date_range) {
      "Error: start date must be before end date"
    }
  })
})
