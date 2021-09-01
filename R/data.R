# CrowdQC+ - Quality control for citizen weather station data.
# Copyright (C) 2021  Daniel Fenner, Tom Grassmann, Benjamin Bechtel, Matthias Demuzere, Jonas Kittner, Fred Meier
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Example CWS data
#'
#' A dataset containing example Netatmo air temperature data from the months
#' June 2017 in Berlin, Germany, and surroundings. The data were obtained by 
#' hourly issuing the 'getpublicdata' function of the Netatmo php
#' application programming interface (API). The requested rectangle was lon NE
#' 13.647242, lat NE 52.727861, and lon SW 12.960249, lat SW 52.340471, split
#' into 64 evenly spaced tiles. Missing values are set to NaN.
#'
#' @format A data frame with 1877040 rows and 6 variables: \describe{
#'   \item{p_id}{a unique identifier for each station, changes if a
#'   station is relocated be the owner}\item{time}{hour were the value belongs
#'   to, in UTC}  \item{ta}{air temperature in degree C} \item{lon}{longitude}
#'   \item{lat}{latitude} \item{z}{height obtained from the nearest pixel from
#'   the Shuttle Radar Topography Mission (SRTM), version 4.1}}
"CWSBer"
