% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{check_species_inspector}
\alias{check_species_inspector}
\title{Gives the inconsistencies between species sampled and species authorized}
\usage{
check_species_inspector(
  dataframe1,
  output,
  species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT")
)
}
\arguments{
\item{dataframe1}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_species_inspector () function.}

\item{output}{{\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".}

\item{species}{{\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). Vector of the species authorized.}
}
\value{
The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
}
\description{
The purpose of the check_species_inspector function is to provide a table of data that contains an inconsistency between the species sampled and species authorized
}
\details{
The input dataframe must contain all these columns for the function to work :
\itemize{
Dataframe 1:
 \item{\code{  samplespecies_id}}
 \item{\code{  species_fao_code}}
}
}
\examples{
#Sample species 1 is ok,
#Sample species 2 has a species outside the list species
dataframe1 <- data.frame(samplespecies_id = c("1", "2"),
                         species_fao_code = c("YFT", "JOS"))
check_species_inspector(dataframe1, output = "report")
}
