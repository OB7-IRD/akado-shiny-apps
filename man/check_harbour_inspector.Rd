% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{check_harbour_inspector}
\alias{check_harbour_inspector}
\title{Gives the inconsistencies between the harbour of landing of the previous trip and the harbour of departure of the current trip}
\usage{
check_harbour_inspector(dataframe1, output)
}
\arguments{
\item{dataframe1}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.}

\item{output}{{\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".}
}
\value{
The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
}
\description{
The purpose of the check_harbour_inspector function is to provide a table of data that contains an inconsistency with the harbour of landing of the previous trip and the harbour of departure of the current trip
}
\details{
The input dataframe must contain all these columns for the function to work :
\itemize{
Dataframe 1:
 \item{\code{  trip_id}}
 \item{\code{  harbour_id_departure_trip_previous}}
 \item{\code{  harbour_name_departure_trip_previous}}
 \item{\code{  harbour_id_landing}}
 \item{\code{  harbour_name_landing}}
}
}
