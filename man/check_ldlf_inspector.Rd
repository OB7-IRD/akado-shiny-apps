% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{check_ldlf_inspector}
\alias{check_ldlf_inspector}
\title{Gives the inconsistencies between the sample measurement types and species or weight values}
\usage{
check_ldlf_inspector(
  dataframe1,
  dataframe2,
  output,
  species = c("SKJ", "LTA", "FRI"),
  size_measure_type_species = c("PD1"),
  size_measure_type_big = c("PD1"),
  size_measure_type_small = c("FL")
)
}
\arguments{
\item{dataframe1}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.}

\item{dataframe2}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.}

\item{output}{{\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".}

\item{species}{{\link[base]{character}} expected. Default values:  c("SKJ", "LTA", "FRI"). Vector of the species not to be associated with a type of measure.}

\item{size_measure_type_species}{{\link[base]{character}} expected. Default values:  c("PD1"). Vector of type of measure not to be associated with species}

\item{size_measure_type_big}{{\link[base]{character}} expected. Default values:  c("PD1"). Type of measure that must have a total weight or a big fish weight}

\item{size_measure_type_small}{{\link[base]{character}} expected. Default values: c("FL"). Type of measure that must have a total weight or a small fish weight}
}
\value{
The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
}
\description{
The purpose of the check_ldlf_inspector  function is to provide a table of data that contains an inconsistency between the sample measurement types and species or weight values
}
\details{
The input dataframe must contain all these columns for the function to work :
\itemize{
Dataframe 1:
 \item{\code{  samplespecies_id}}
 \item{\code{  specie_name}}
 \item{\code{  sizemeasuretype_code}}
 \item{\code{  sample_id}}
Dataframe 2:
 \item{\code{  sample_id}}
 \item{\code{  sample_smallsweight}}
 \item{\code{  sample_bigsweight}}
 \item{\code{  sample_totalweight}}
}
}
