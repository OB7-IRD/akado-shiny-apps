% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{data_button_plot}
\alias{data_button_plot}
\title{Create the button in the table}
\usage{
data_button_plot(
  id,
  data,
  colname_id,
  colname_info,
  name_button = NULL,
  choice_select_row = "error"
)
}
\arguments{
\item{id}{{\link[base]{character}} expected. Button identifier}

\item{data}{{\link[base]{data.frame}} expected. data frame where the button will be added}

\item{colname_id}{{\link[base]{character}} expected. Name of column used as unique identifier for data rows}

\item{colname_info}{{\link[base]{character}} expected. Name of data columns to be transmitted via button name}

\item{name_button}{{\link[base]{character}} expected. Default values: NULL, allows you to specify a name for the button, otherwise use the prefix "button_" and the supplied id}

\item{choice_select_row}{{\link[base]{character}} expected. Default values: "error", the possible values are "all" to create a button on all lines, "error" to create a button only on lines considered inconsistent, "valid" to create a button only on lines considered consistent}
}
\value{
The function returns a {\link[base]{data.frame}}
}
\description{
Shiny function : Function to create the button in the table that will create the plot
}
