#' Example CWS data
#'
#' The example data set contains one month of randomly created data for 624 'crowd weather stations' CWS.
#' The air-temperature data is based on measured air temperature at one location and
#' then randomly creating a sort-of meaningful spatially-distributed data set out of it.
#'
#' @format A data table with 298706 rows and 6 variables: 
#' \describe{
#'   \item{p_id}{Unique identifier for each CWS}
#'   \item{time}{Time stamp to which the air-temperature data corresponds to, in UTC}
#'   \item{ta}{Air temperature in degree C} 
#'   \item{lon}{Longitude of the CWS (WGS-84)}
#'   \item{lat}{Latitude of the CWS (WGS-84)}
#'   \item{z}{Altitude (m) of the CWS}}
"cqcp_cws_data"