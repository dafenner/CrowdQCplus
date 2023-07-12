# CrowdQC+ - Quality control for citizen weather station data.
# Copyright (C) 2021-2023  Daniel Fenner, Tom Grassmann, Benjamin Bechtel, Matthias Demuzere, Jonas Kittner, Fred Meier
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

#' cqcp_colourise
#' 
#' Colours a string with ANSI escapes.
#'
#' @param str A string.
#' @param colour "red", "green", or "yellow"
#'   This can be, e.g., '10 days'.
#'
#' @return coloured string
cqcp_colourise <- function(str, colour) {
  # ANSI escapes
  escape_sequences <- list(
    red = "\u001b[31m",
    green = "\u001b[32m",
    yellow = "\u001b[33m")
  reset = '\u001b[0m'
  str_col <- paste0(escape_sequences[[colour]], str, reset, sep='')
  return(str_col)
}

#' Input check of data for CrowdQC+
#' 
#' Checks if a data.table has the required format for CrowdQC+ and gives hints
#' regarding the application of the package.
#' 
#' 1. Check for column names and respective data types.
#' 2. Check for same temporal coverage.
#' 3. Check for data regularity.
#' 4. Geographical extent of data.
#' 5. Number of stations. 
#'
#' @param data data.table to be checked.
#' @param print Set to TRUE to print information in the console. Default: TRUE
#' @param file Provide a file path to store the information in a file.
#' @param as_list Return information from the checks as a list, instead of simple 
#'   overall TRUE/FALSE. This can be helpful in automatic work flows.
#'
#' @return logical (or list), TRUE if data passed all checks.
#' @export
cqcp_check_input <- function(data, print = TRUE, file = NULL, as_list = FALSE){
  
  ok <- TRUE
  ch_1a <- ch_1b <- ch_1c <- ch_2 <- ch_3 <- ch_4 <- ch_5 <- TRUE
  mess_2_l <- mess_3_l <- mess_4_l <- mess_5_l <- "OK"
 
  # (1) Check for column names
  # (1a) Required columns
  has_p_id <- cqcp_has_column(data, column = "p_id")
  has_time <- cqcp_has_column(data, column = "time")
  has_ta <- cqcp_has_column(data, column = "ta")
  has_lon <- cqcp_has_column(data, column = "lon")
  has_lat <- cqcp_has_column(data, column = "lat")
  miss_1a <- c()
  if(!has_p_id | !has_time | !has_ta | !has_lon | !has_lat) {
    if(!has_p_id) miss_1a <- c(miss_1a, "p_id")
    if(!has_time) miss_1a <- c(miss_1a, "time")
    if(!has_ta) miss_1a <- c(miss_1a, "ta")
    if(!has_lon) miss_1a <- c(miss_1a, "lon")
    if(!has_lat) miss_1a <- c(miss_1a, "lat")
    mess_1a <- cqcp_colourise(paste0("     ! Missing: ", paste(miss_1a, collapse = ", "),"\n     --> CrowdQC+ will not work with this data.\n"), "red")
    ch_1a <- FALSE
    ok <- FALSE
  } else mess_1a <- cqcp_colourise("     OK\n", "green")
  # (1b) Optional columns
  has_z <- cqcp_has_column(data, column = "z")
  miss_1b <- c()
  if(!has_z) {
    miss_1b <- c(miss_1b, "z")
    mess_1b <- cqcp_colourise("     ! Missing: z\n", "yellow")
    mess_1b <- c(mess_1b, cqcp_colourise("     --> Filters cqcp_m2 and cqcp_m5 will not work with 'heightCorrection = T'. You can run 'cqcp_add_dem_height' to add DEM information.\n", "yellow"))
    ch_1b <- FALSE
  } else mess_1b <- cqcp_colourise("     OK\n", "green")
  # (1c) Check for correct data type
  if (has_p_id) type_p_id <- (is.integer(data$p_id) | is.character(data$p_id)) else type_p_id <- FALSE
  if (has_time) type_time <- lubridate::is.POSIXct(data$time) else type_time <- FALSE
  if (has_ta) type_ta <- is.numeric(data$ta) else type_ta <- FALSE
  if (has_lon) type_lon <- is.numeric(data$lon) else type_lon <- FALSE
  if (has_lat) type_lat <- is.numeric(data$lat) else type_lat <- FALSE
  if (has_z) type_z <- is.numeric(data$z) else type_z <- TRUE
  wrong_1c <- c()
  if(!type_p_id | !type_time | !type_ta | !type_lon | !type_lat | !type_z) {
    if(!type_p_id) wrong_1c <- c(wrong_1c, "p_id")
    if(!type_time) wrong_1c <- c(wrong_1c, "time")
    if(!type_ta) wrong_1c <- c(wrong_1c, "ta")
    if(!type_lon) wrong_1c <- c(wrong_1c, "lon")
    if(!type_lat) wrong_1c <- c(wrong_1c, "lat")
    if(!type_z) wrong_1c <- c(wrong_1c, "z")
    if ("z" %in% wrong_1c) {
      if (length(wrong_1c) == 1) {
        mess_1c <- cqcp_colourise(paste0("     ! Wrong data type: ", paste(wrong_1c, collapse = ", "),"\n     --> Filters cqcp_m2 and cqcp_m5 will not work with 'heightCorrection = T'\n"), "yellow")
      } else {
        mess_1c <- cqcp_colourise(paste0("     ! Wrong data type: ", paste(wrong_1c, collapse = ", "),"\n     --> CrowdQC+ will not work with this data.\n"), "red")
        ok <- FALSE
      }
    } else {
      mess_1c <- cqcp_colourise(paste0("     ! Wrong data type: ", paste(wrong_1c, collapse = ", "),"\n     --> CrowdQC+ will not work with this data.\n"), "red")
      ok <- FALSE
    }
  } else mess_1c <- cqcp_colourise("     OK\n", "green")
  
  # (2) Check for same temporal coverage.
  if(has_time & has_p_id) {
    setkey(data, p_id, time)
    p1 <- data[1]$p_id
    t1 <- data[p_id == p1]$time
    test2 <- all(data[, identical(time, t1), by = p_id][, V1])
    if(test2) {
      mess_2 <- cqcp_colourise("     OK\n", "green")
    } else {
      mess_2 <- cqcp_colourise("     ! Temporal coverage not the same for all p_id.\n     --> You can run 'cqcp_padding' to make your data regular.\n", "red")
      mess_2_l <- "temporal coverage not identical"
      ch_2 <- FALSE
      ok <- FALSE
    }
  } else {
    mess_2 <- cqcp_colourise("     ! Columns needed for this check: 'p_id', 'time'. See Check 1a what is missing.\n", "red")
    mess_2_l <- "columns missing (cf. 'check_1a_missing')"
    ch_2 <- FALSE
    ok <- FALSE
  }
  
  # (3) Check for data regularity.
  if(has_time & has_p_id) {
    setkey(data, p_id, time)
    dt <- data[, length(unique(diff(time))) == 1, keyby = p_id][, V1] # data regular
    if(all(dt)) {
      mess_3 <- cqcp_colourise("     OK\n", "green")
    } else {
      mess_3 <- cqcp_colourise("     ! Data not regular for all p_id.\n     --> You can run 'cqcp_padding' to make your data regular.\n", "red")
      mess_3_l <- "data irregular"
      ch_3 <- FALSE
      ok <- FALSE
    }
  } else {
    mess_3 <- cqcp_colourise("     ! Columns needed for this check: 'p_id', 'time'. See Check 1a what is missing.\n", "red")
    mess_3_l <- "columns missing (cf. 'check_1a_missing')"
    ch_3 <- FALSE
    ok <- FALSE
  }
    
  # (4) Geographical extent
  if(has_p_id & has_lon & has_lon) {
    p_id <- unique(data$p_id)
    loc <- data[, .SD[1], by = p_id, .SDcols = c("lon", "lat")][,c("lon", "lat")]
    dist <- as.matrix(terra::distance(as.matrix(loc), lonlat = TRUE)) # calculate distances between points
    max_dist <- max(dist, na.rm = T)
    if(max_dist > 141421.4) {
      mess_4 <- cqcp_colourise("     ! Geographic extent is large (> 100 km x 100 km).\n     --> You might want to split your data into smaller regions.\n", "yellow")
      mess_4_l <- "large geographical extent"
    } else {
      mess_4 <- cqcp_colourise("     OK\n", "green")
    }
  } else {
    mess_4 <- cqcp_colourise("     ! Columns needed for this check: 'p_id', 'lon', 'lat'.\n     --> See Check 1a what is missing.\n", "red")
    mess_4_l <- "columns missing (cf. 'check_1a_missing')"
    max_dist <- NA
    ch_4 <- FALSE
    ok <- FALSE
  }
  
  # (5) Number of stations/values.
  if(has_p_id & has_time & has_ta) {
    n_pid <- data[, sum(!is.na(ta)), by = time][, median(V1, na.rm = T)] # median value of available stations per time step
    if(n_pid < 100) {
      mess_5 <- cqcp_colourise(paste0("     ! Low number of stations with non-NA values per time step (median = ",n_pid,").\n     --> Usage of 't_distribution = T' in filter cqcp_m2 is recommended.\n"), "yellow")
      mess_5_l <- "low number of stations"
    } else {
      mess_5 <- cqcp_colourise("     OK\n", "green")
    }
  } else {
    mess_5 <- cqcp_colourise("     ! Columns needed for this check: 'time', 'ta'.\n     --> See Check 1a what is missing.\n", "red")
    mess_5_l <- "columns missing (cf. 'check_1a_missing')"
    n_pid <- NA
    ch_5 <- FALSE
    ok <- FALSE
  }
  
  # Print
  if(print) {
    cat("+++++++++++++++++++++++++++++\n")
    cat("+ CrowdQC+ input data check +\n")
    cat("+++++++++++++++++++++++++++++\n")
    # (1)
    cat("Check 1a - Required columns:\n")
    cat(mess_1a)
    cat("Check 1b - Optional columns:\n")
    cat(mess_1b)
    cat("Check 1c - Column data types:\n")
    cat(mess_1c)
    # (2)
    cat("Check 2 - Temporal coverage:\n")
    cat(mess_2)
    # (3)
    cat("Check 3 - Regularity:\n")
    cat(mess_3)
    # (4)
    cat("Check 4 - Geographical extent:\n")
    cat(mess_4)
    # (5)
    cat("Check 5 - Number of stations:\n")
    cat(mess_5)
  }
  
  # File?
  if (!is.null(file)) {
    sink(file)
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat("++++  CrowdQC+ input data check  ++++\n")
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat(paste0("File created: ",lubridate::now("UTC")," UTC\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat(paste0("R variable name: ",deparse(substitute(data)),"\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    # (1)
    cat("Check 1a - Required columns:\n")
    cat(substr(mess_1a, 6, nchar(mess_1a)-4))
    cat("Check 1b - Optional columns:\n")
    cat(substr(mess_1b, 6, nchar(mess_1b)-4))
    cat("Check 1c - Column data types:\n")
    cat(substr(mess_1c, 6, nchar(mess_1c)-4))
    # (2)
    cat("Check 2 - Temporal coverage:\n")
    cat(substr(mess_2, 6, nchar(mess_2)-4))
    # (3)
    cat("Check 3 - Regularity:\n")
    cat(substr(mess_3, 6, nchar(mess_3)-4))
    # (4)
    cat("Check 4 - Geographical extent:\n")
    cat(substr(mess_4, 6, nchar(mess_4)-4))
    # (5)
    cat("Check 5 - Number of stations:\n")
    cat(substr(mess_5, 6, nchar(mess_5)-4))
    sink()
  }
  
  if(as_list) {
    out_list <- list("check" = ok, "check_1a_flag" = ch_1a, "check_1a_missing" = miss_1a,
                     "check_1b_flag" = ch_1b, "check_1b_missing" = miss_1b,
                     "check_1c_flag" = ch_1c, "check_1c_wrong" = wrong_1c,
                     "check_2_flag" = ch_2, "check_2_message" = mess_2_l,
                     "check_3_flag" = ch_3, "check_3_message" = mess_3_l,
                     "check_4_flag" = ch_4, "check_4_message" = mess_4_l, 
                     "check_4_max_distance_m" = max_dist,
                     "check_5_flag" = ch_5, "check_5_message" = mess_5_l,
                     "check_5_median_stations" = n_pid)
    return(out_list)
  } else return(ok)
}

#' Extract raster value(s) at given position(s) from a SpatRaster object or geotiff
#' file.
#'
#' @param lon Longitude values(s)
#' @param lat Latitude values(s) 
#' @param raster SpatRaster object (cf. terra package) 
#' @param file Path to a geotiff file
#'
#' @return extracted value(s) for given lon/lat
cqcp_extract_raster_data <- function(lon, lat, raster = NULL, file = NULL){
  
  # Input
  if(!is.null(raster)) {
    gtiff <- raster
  } else if(!is.null(file)) {
    gtiff <- terra::rast(file) # Open GeoTIFF
  } else stop("[CrowdQC+] No input given as 'file' or 'raster'.")
  
  # Convert lon/lat to "SpatialPoints"
  coords <- data.frame(lon=lon, lat=lat)
  coords <- terra::vect(coords, crs = "EPSG:4326") # WGS-84
  
  # Transform to CRS of raster
  coords <- terra::project(coords, terra::crs(gtiff))
  
  # Extract data for each location
  value <- as.numeric(terra::extract(gtiff, coords, ID = FALSE, raw = TRUE))
  
  return(value) # Output
}

#' Download SRTM data using geodata package for bounding box of data.
#' 
#' SRTM data is automatically downloaded and, in case of more than one SRTM tile, 
#' merged together as a mosaic.
#' The SRTM SpatRaster object can be cropped to the geographical extent of 
#' the data (crop = TRUE).
#' SRTM source: https://srtm.csi.cgiar.org/
#'
#' @param data data.table/data.frame with at least columns 'lon' and 'lat' 
#' @param directory Directory path to optionally store the SRTM data. If NULL,
#'   downloaded data is stored in the current working directory. 
#' @param outfile File path to save the SRTM raster as geotiff 
#' @param overwrite Overwrite existing geotiff? Default: TRUE. 
#' @param crop Crop raster/geotiff to data extent? Default: FALSE. 
#' @param method Download method for geodata::elevation_3s. Use default here to 
#'    avoid SSL certificate issues (cf. https://github.com/rspatial/geodata/issues/39#issuecomment-1404513208). 
#'    Default: "curl".
#' @param extra Additional keyword for "curl" method. Use default here to avoid SSL
#'    certificate issues (cf. https://github.com/rspatial/geodata/issues/39#issuecomment-1404513208). 
#'    Default: "-k"
#' @param ... Additional parameters supported by geodata::elevation_3s
#'
#' @return SpatRaster object with SRTM data
#' @export
cqcp_download_srtm <- function(data, directory = NULL, outfile = NULL, 
                               overwrite = TRUE, crop = FALSE, 
                               method = "curl", extra = "-k", ...){
  
  # Check if directory exists
  if(!is.null(directory)) {
    if(!dir.exists(directory)) dir.create(directory)
  } else directory = getwd()
  
  # Get bounding box coordinates
  min_lon <- min(data$lon)
  max_lon <- max(data$lon)
  min_lat <- min(data$lat)
  max_lat <- max(data$lat)
  
  # Download data
  ll <- geodata::elevation_3s(lat = min_lat, lon = min_lon, path = directory, method = method, extra = extra, ...)
  lr <- geodata::elevation_3s(lat = min_lat, lon = max_lon, path = directory, method = method, extra = extra, ...)
  ur <- geodata::elevation_3s(lat = max_lat, lon = max_lon, path = directory, method = method, extra = extra, ...)
  ul <- geodata::elevation_3s(lat = max_lat, lon = min_lon, path = directory, method = method, extra = extra, ...)
  
  # Combine tiles, if necessary
  mosaic <- terra::mosaic(ll, lr, ur, ul, fun=mean)
  
  # Crop to data extent
  if(crop) {
    data_extent <- terra::ext(min_lon, max_lon, min_lat, max_lat)
    mosaic <- terra::crop(mosaic, data_extent)
  }
  
  # Store merged raster?
  if(!is.null(outfile)) {
    if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile))
    terra::writeRaster(mosaic, filename = outfile, overwrite = overwrite)
  }
  
  return(mosaic)
}

#' Add DEM height to data.table.
#' 
#' Adds Digital Elevation Model (DEM) information from a SpatRaster object or 
#' geotiff to the data.table.
#' If no SpatRaster object or file are given as parameters, SRTM data 
#' (Jarvis et al. 2008) is automatically downloaded and used to extract DEM 
#' information at the positions of the stations. Keep in mind that SRTM data is 
#' only available for regions between 60° N and 56° S. For regions outside that 
#' range, or if the user wants to use another DEM, that DEM can be used as input 
#' via the 'file' parameter (path to geotiff) or via the 'raster' parameter 
#' (SpatRaster object).
#' 
#' A new column 'z' is added to the data.table to be used in QC level m2.
#' 
#' Reference: 
#' Jarvis A., Reuter, H. I., Nelson, A. and Guevara, E. (2008): Hole-filled 
#' seamless SRTM data V4, International Centre for Tropical Agriculture (CIAT), 
#' available from https://srtm.csi.cgiar.org
#'
#' @param data data.table/frame with at least columns 'lon' and 'lat' 
#' @param file Path to a DEM geotiff 
#' @param raster SpatRaster object with DEM data 
#' @param directory Directory path to store the downloaded SRTM data. If NULL,
#'   downloaded data is stored in the current working directory. 
#' @param outfile File path to save the created SRTM raster as geotiff 
#' @param overwrite Overwrite existing geotiff? Default: TRUE.
#' @param crop Crop SRTM raster/geotiff to data extent. Default: FALSE.
#' @param na_vals Set NA values in DEM to this value to avoid missing value in 
#'   cqcp_m2. Default: 0
#' @param overwrite_z If column 'z' exists in data, should it be overwritten? Default: TRUE.
#' @param quiet Suppress messages by CrowdQC+. Default: FALSE
#' @param ... Additional parameters supported by geodata::elevation_3s
#'
#' @return data table with column 'z' with DEM information
#' @export
cqcp_add_dem_height <- function(data, file = NULL, raster = NULL, 
                                directory = NULL, outfile = NULL, 
                                overwrite = TRUE, crop = FALSE, 
                                na_vals = 0, quiet = FALSE, 
                                overwrite_z = TRUE, ...){
  
  # Extract unique locations from data
  locations <- data[, .SD[1], by = p_id, .SDcols = c("lon", "lat")]
  
  # Download SRTM data in case no file or raster is given
  if(is.null(raster) & is.null(file)){
    if((min(locations$lat) < -56 | max(locations$lat) > 60) & !quiet) {
      cat(cqcp_colourise("[CrowdQC+] Some stations are outside SRTM data coverage. Recommend to use another DEM of your choice via parameters 'file' or 'raster'.\n", "yellow"))
    }
    raster <- cqcp_download_srtm(locations, directory = directory, 
                                 outfile = outfile, overwrite = overwrite, 
                                 crop = crop, ...)
  }
  
  # Get height data per site
  height <- cqcp_extract_raster_data(locations$lon, locations$lat, 
                                     raster = raster, file = file)
  
  # deal with NA values
  height[is.na(height)] <- na_vals # should we change to default NA?
  
  # Put information back together
  locations <- cbind(locations, z = height)
  if(cqcp_has_column(data, column = "z") & overwrite_z) data[, z := NULL]
  data_out <- merge(data, locations[, .(p_id, z)], all.x = T, by = "p_id", sort = FALSE)
  
  return(data_out)
}

#' Padding of data for application of CrowdQC+
#' 
#' CrowdQC+ (as CrowdQC) requires that all stations in the data.table have the 
#' same length and the same temporal resolution. Missing times are set to NaN.
#' This can be done using cqcp_padding. Currently, time stamps are rounded and
#' no interpolation is carried out to the data itself.
#'
#' @param data data.table with at least columns 'p_id', 'time, 'ta' 
#' @param resolution Temporal resolution as supported by lubridate. Default: '1 hour'
#' @param rounding_method Method to apply to round the time values to (cf. lubridate). 
#'   Default is 'nearest', to assign the values to the nearest full time value, 
#'   defined by 'resolution'. Other options are 'ceiling'/'ceil' and 'floor'.
#' @param quiet Suppress messages by CrowdQC+. Default: FALSE
#'
#' @return data.table with regular time series for all stations.
#' @export
cqcp_padding <- function(data, resolution = "1 hour", rounding_method = "nearest",
                         quiet = FALSE) {
  
  # parameters & input
  is_ceiling <- is_floor <- is_nearest <- FALSE
  
  time_step <- lubridate::duration(resolution)
  in_data <- copy(data)

  # Make time stamps regular
  if (toupper(rounding_method) == "CEILING" | toupper(rounding_method) == "CEIL") { 
    is_ceiling <- TRUE # round time values to next full time, default
  } else if (toupper(rounding_method) == "FLOOR") { 
    is_floor <- TRUE # round time values to previous full time
  } else if (toupper(rounding_method) == "NEAREST" | toupper(rounding_method) == "NEAR") { 
    is_nearest <- TRUE # round time values to nearest full time
  } else {
    if(!quiet) cat(cqcp_colourise(paste0(" [CrowdQC+] Rounding method '",rounding_method,"' not supported. See documentation.\n", "yellow")))
    return(data)
  }

  # Time rounding
  if (is_ceiling) { 
    in_data[, time := lubridate::ceiling_date(time, unit = resolution)]
  } else if (is_floor) { 
    in_data[, time := lubridate::floor_date(time, unit = resolution)]
  } else if (is_nearest) { 
    in_data[, time := lubridate::round_date(time, unit = resolution)]
  }
  
  # calculate mean and select first instance of all columns in case of multiple 
  # values per time stamp
  in_data[, `:=`(ta, mean(ta, na.rm = T)), by = .(p_id, time)][,.SD[1], by = .(p_id, time)]
  
  # Create regular time series
  m0 = min(in_data$time)
  m1 = max(in_data$time)
  full_time <- seq(m0, m1, time_step)
  
  # Get columns
  columns <- colnames(in_data)
  columns <- columns[which(columns != "p_id" & columns != "time" & columns != "ta")]

  # Get unique stations and create new "big" data.table
  info <- in_data[, .SD[1], .SDcols = columns, by = p_id]
  full_d <- data.table(expand.grid(p_id = info$p_id, time = full_time))
  
  # Merge tables
  data_full <- merge(full_d, info, all.x = T, by = "p_id")
  columns2 <- c("p_id", "time", "ta")
  data_pp <- merge(data_full, in_data[, ..columns2], all.x = T, by = c("p_id", "time"))
  
  setkey(data_pp, p_id, time)
  
  return(data_pp)
}

#' Simple statistics on data availability and number of stations after CrowdQC+.
#' 
#' Calculate how many valid values and unique stations with valid data remain 
#' after each QC level and print the information.
#'
#' @param data data.table after CrowdQC+
#' @param print Set to TRUE to print information in the console. Default: TRUE
#' @param file Provide a file path to store the information in a file.
#'
#' @return data.table with output statistics
#' @export
cqcp_output_statistics <- function(data, print = TRUE, file = NULL) {
  
  levels <- c("m1", "m2", "m3", "m4", "m5", "o1", "o2", "o3")
  
  n_data <- data.table()
  n_data[, n_raw := data[, sum(!is.na(ta))]]
  n_stat <- data.table()
  n_stat[, n_raw := data[, !(sum(is.na(ta)) == .N), by = .(p_id)][, sum(V1)]]
  
  for(i in levels) {
    if(cqcp_has_column(data, column = i)) {
      n_data[, (i) := data[, sum(get(i))]]
      n_stat[, (i) := data[, !(sum(get(i) == F) == .N), by = .(p_id)][, sum(V1)]]
    }
  }
  
  if(print) {
    columns <- colnames(n_data)
    cat("++++++++++++++++++++++++++++++\n")
    cat("+ CrowdQC+ output statistics +\n")
    cat("++++++++++++++++++++++++++++++\n")
    cat(paste0("Raw data: ",n_data$n_raw," values, ",n_stat$n_raw," stations\n"))
    for(j in 2:length(columns)) {
      cat(paste0("QC level ",columns[j],": ",n_data[,as.character(get(columns[j]))],
                 " values (= ",sprintf("%.2f",n_data[,(get(columns[j]))]/n_data$n_raw*100),
                 " % of raw data), ",n_stat[,as.character(get(columns[j]))]," stations\n"))
    }
  }
  # File?
  if (!is.null(file)) {
    columns <- colnames(n_data)
    sink(file)
    cat("++++++++++++++++++++++++++++++\n")
    cat("+ CrowdQC+ output statistics +\n")
    cat("++++++++++++++++++++++++++++++\n")
    cat(paste0("File created: ",lubridate::now("UTC")," UTC\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat(paste0("R variable name: ",deparse(substitute(data)),"\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat(paste0("Raw data: ",n_data$n_raw," values, ",n_stat$n_raw," stations\n"))
    for(j in 2:length(columns)) {
      cat(paste0("QC level ",columns[j],": ",n_data[,as.character(get(columns[j]))],
                 " values (= ",sprintf("%.2f",n_data[,(get(columns[j]))]/n_data$n_raw*100),
                 " % of raw data), ",n_stat[,as.character(get(columns[j]))]," stations\n"))
    }
    sink()
  }
  return(n_data)
}
