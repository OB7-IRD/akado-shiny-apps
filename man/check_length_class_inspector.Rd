% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{check_length_class_inspector}
\alias{check_length_class_inspector}
\title{Gives the inconsistencies between size class of the samples depending on the species and measurement type and the valid limits}
\usage{
check_length_class_inspector(
  dataframe1,
  output,
  species = c("YFT", "BET", "ALB"),
  size_measure_type = "FL",
  limit = 80
)
}
\arguments{
\item{dataframe1}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weighting_inspector () function.}

\item{output}{{\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".}

\item{species}{{\link[base]{character}} expected. Default values: c("YFT", "BET", "ALB"). Vector of the species inventory controlled.}

\item{size_measure_type}{{\link[base]{character}} expected. Default values: "FL". Vector of the size measure type controlled.}

\item{limit}{{\link[base]{numeric}} expected. Default values: 80. Limit of the size measure}
}
\value{
The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
}
\description{
The purpose of the check_length_class_inspector function is to provide a table of data that contains an inconsistency between the size class of the samples depending on the species and measurement type and the valid limits (Default : 80)
}
\details{
The input dataframe must contain all these columns for the function to work :
\itemize{
Dataframe 1:
 \item{\code{  samplespeciesmeasure_id}}
 \item{\code{  specie_name}}
 \item{\code{  sizemeasuretype_code}}
 \item{\code{  samplespeciesmeasure_sizeclass}}
}
}
