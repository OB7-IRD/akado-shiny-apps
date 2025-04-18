% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function.R
\name{check_eez_inspector}
\alias{check_eez_inspector}
\title{Gives the inconsistencies between the fishing area declared and calculated for the activity}
\usage{
check_eez_inspector(
  dataframe1,
  dataframe2,
  output,
  activity_crs = 4326,
  international_waters_code = "XIN",
  vessel_activity = c("6")
)
}
\arguments{
\item{dataframe1}{{\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_weight_inspector () function.}

\item{dataframe2}{{\link[base]{data.frame}} expected. Layer to containing the eez shapefile (example cf : Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632)}

\item{output}{{\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".}

\item{activity_crs}{{\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity}

\item{international_waters_code}{{\link[base]{character}} expected. Default values: "XIN". iso3 code corresponding to international waters}

\item{vessel_activity}{{\link[base]{character}} expected. Default values: c("6"). Vector of inventory of codes for activities that must have a zee zone}
}
\value{
The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
}
\description{
The purpose of the check_eez_inspector function is to provide a table of data that contains an inconsistency between the fishing area declared and calculated with position for the activity fishing
}
\details{
The input dataframe must contain all these columns for the function to work :
\itemize{
Dataframe 1:
 \item{\code{  activity_id}}
 \item{\code{  vesselactivity_code}}
 \item{\code{  fpazone_code}}
 \item{\code{  fpazone_country_iso3}}
 \item{\code{  activity_position}}
}
\itemize{
Dataframe 2:
 \item{\code{  ISO_TER1}}
 \item{\code{  ISO_TER2}}
 \item{\code{  ISO_TER3}}
 \item{\code{  geometry}}
}
}
\examples{
#Activity 1, 2, 3 and 5 are ok,
#Activity 4 is outside the EEZ delimited in the shapefile,
#Activity 6 has a missing EEZ zone and the vessel's activity is in vessel_activity,
#Activity 7 has different EEZ,
#Activity 8 has an EEZ zone unknown to the shapefile and which also does not exist in
#           international_waters_code
dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
                         vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"),
                         fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"),
                         fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"),
                         activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)",
                                               "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)",
                                               "POINT (1 1)", "POINT (6 6)"))
dataframe2 <- sf::st_sf(data.frame(ISO_TER1 = c("SYC", "XSG"),
                                   ISO_TER2 = c(NA, NA),
                                   ISO_TER3 = c(NA, NA),
                                   geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
                                                                                   c(2,2), c(0,2),
                                                                                   c(0,0)))),
                                                         sf::st_polygon(list(rbind(c(3,3), c(3,5),
                                                                                   c(5,5), c(5,3),
                                                                                   c(3,3)))),
                                                         crs = 4326)))
check_eez_inspector(dataframe1, dataframe2, output = "report")
}
