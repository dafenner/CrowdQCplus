#' Example Netatmo data
#'
#' A dataset containing example Netatmo air temperature data from the months
#' January and June 2017 in Berlin, Germany, and surroundings. The data were
#' obtained by hourly issueing the 'getpublicdata' function of the Netatmo php
#' application programming interface (API). The requested rectangle was lon NE
#' 13.647242, lat NE 52.727861, and lon SW 12.960249, lat SW 52.340471, split
#' into 64 evenly spaced tiles. Missing values are set to NaN.
#'
#' @format A data frame with 3770520 rows and 6 variables: \describe{
#'   \item{p_id}{a unique identifier for each Netatmo station, changes if a
#'   station is relocated be the owner}\item{time}{hour were the value belongs
#'   to, in UTC}  \item{ta}{air temperature in degree C} \item{lon}{longitude}
#'   \item{lat}{latitude} \item{z}{height obtained from the nearest pixel from
#'   the Shuttle Radar Topography Mission (SRTM), version 4.1}}
"netatmoBer"
