
#' Input check of data for CrowdQC+
#' 
#' Checks if a data.table has the required format for CrowdQC+ and gives hints
#' regarding the application of the package.
#' 
#' 1. Check for column names.
#' 2. Check for data regularity.
#' 3. Geographical extent of data.
#' 4. Number of stations. 
#'
#' @param data data.table to be checked.
#' @param print Set to TRUE to print information in the console. Default: TRUE
#' @param file Provide a file path to store the information in a file.
#'
#' @return logical, TRUE if data passed all checks.
cqcp_check_input <- function(data, print = TRUE, file = NULL){
  
  ok <- TRUE
 
  # (1) Check for column names
  # required
  has_p_id <- cqcp_has_column(data, column = "p_id")
  has_time <- cqcp_has_column(data, column = "time")
  has_ta <- cqcp_has_column(data, column = "ta")
  has_lon <- cqcp_has_column(data, column = "lon")
  has_lat <- cqcp_has_column(data, column = "lat")
  # optional
  has_z <- cqcp_has_column(data, column = "z")
  
  # (2) Check for data regularity.
  if(has_time & has_p_id) {
    setkey(data, p_id, time)
    dt <- data[, length(unique(diff(time))) == 1, keyby=p_id] # data regular
    if(all(dt)) {
      mess_2 <- "     OK\n"
    } else {
      mess_2 <- "     ! Data not regular for all p_id.\n     --> You can run 'cqcp_padding' to make your data regular.\n"
      ok <- FALSE
    }
  } else {
    mess_2 <- "     ! Columns needed for this check: 'p_id', 'time'. See Check 1a what is missing.\n"
    ok <- FALSE
  }
    
  # (3) Geographical extent
  if(has_p_id & has_lon & has_lon) {
    p_id <- unique(data$p_id)
    loc <- data[, .SD[1], by = p_id, .SDcols = c("lon", "lat")][,c("lon", "lat")]
    dist <- raster::pointDistance(loc, lonlat=TRUE) # calculate distances between points
    if(max(dist, na.rm = T) > 141421.4) {
      mess_3 <- "     ! Geographic extend is large (> 100 km x 100 km).\n     --> You might want to split your data into smaller regions.\n"
      ok <- FALSE
    } else {
      mess_3 <- "     OK\n"
    }
  } else {
    mess_3 <- "     ! Columns needed for this check: 'p_id', 'lon', 'lat'.\n     --> See Check 1a what is missing.\n"
    ok <- FALSE
  }
  
  # (4) Number of stations.
  if(has_p_id) {
    pid <- unique(data$p_id)
    if(length(pid) < 200) {
      mess_4 <- paste0("     ! Low number of stations (",length(pid),").\n     --> Usage of 'fun=qt' in filter M2 is recommended.\n")
      ok <- FALSE
    } else {
      mess_4 <- "     OK\n"
    }
  } else {
    mess_4 <- "     ! Column needed for this check: 'p_id'.\n"
    ok <- FALSE
  }
  
  # Print
  if(print) {
    cat("+++++++++++++++++++++++++++++\n")
    cat("+ CrowdQC+ input data check +\n")
    cat("+++++++++++++++++++++++++++++\n")
    # (1)
    cat("Check 1a - Required columns:\n")
    if(!has_p_id | !has_time | !has_ta | !has_lon | !has_lat) {
      miss <- c()
      if(!has_p_id) miss <- c(miss, "p_id")
      if(!has_time) miss <- c(miss, "time")
      if(!has_ta) miss <- c(miss, "ta")
      if(!has_lon) miss <- c(miss, "lon")
      if(!has_lat) miss <- c(miss, "lat")
      cat("     ! Missing: ", miss, "\n     --> CrowdQC+ will not work with this data.\n")
      ok <- FALSE
    } else cat("     OK\n")
    cat("Check 1b - Optional columns:\n")
    if(!has_z) {
      cat("     ! Missing: z\n")
      cat("     --> Filters M2 and M5 will not work with 'heightCorrection'. You can run 'cqcp_add_dem_height' to add DEM information.\n")
    } else cat("     OK\n")
    # (2)
    cat("Check 2 - Regularity:\n")
    cat(mess_2)
    # (3)
    cat("Check 3 - Geographical extent:\n")
    cat(mess_3)
    # (4)
    cat("Check 4 - Number of stations:\n")
    cat(mess_4)
  }
  
  # File?
  if (!is.null(file)) {
    
    sink(file)
    
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat("++++  CrowdQC+ input data check  ++++\n")
    cat("+++++++++++++++++++++++++++++++++++++\n")
    
    cat(paste0("File created: ",now("UTC")," UTC\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    cat(paste0("R variable name: ",deparse(substitute(data)),"\n"))
    cat("+++++++++++++++++++++++++++++++++++++\n")
    
    # (1)
    cat("Check 1a - Required columns:\n")
    if(!has_p_id | !has_time | !has_ta | !has_lon | !has_lat) {
      miss <- c()
      if(!has_p_id) miss <- c(miss, "p_id")
      if(!has_time) miss <- c(miss, "time")
      if(!has_ta) miss <- c(miss, "ta")
      if(!has_lon) miss <- c(miss, "lon")
      if(!has_lat) miss <- c(miss, "lat")
      cat("     ! Missing: ", miss, "\n     --> CrowdQC+ will not work with this data.\n")
      ok <- FALSE
    } else cat("     OK\n")
    cat("Check 1b - Optional columns:\n")
    if(!has_z) {
      cat("     ! Missing: z\n")
      cat("     --> Filters M2 and M5 will not work with 'heightCorrection'. You can run 'cqcp_add_dem_height' to add DEM information.\n")
    } else cat("     OK\n")
    # (2)
    cat("Check 2 - Regularity:\n")
    cat(mess_2)
    # (3)
    cat("Check 3 - Geographical extent:\n")
    cat(mess_3)
    # (4)
    cat("Check 4 - Number of stations:\n")
    cat(mess_4)
    
    sink()
  }
  
  return(ok)
}

#' Extract raster value at given position(s) for a RasterLayer Object or geotiff
#' file.
#'
#' @param lon longitude values(s)
#' @param lat latitude values(s) 
#' @param raster RasterLayer object (cf. raster package) 
#' @param file path to a geotiff file
#'
#' @return extracted value(s) for given lon/lat
cqcp_extract_raster_data <- function(lon, lat, raster = NULL, file = NULL){
  
  # Input
  if(!is.null(raster)) {
    gtiff <- raster
  } else if(!is.null(file)) {
    gtiff <- raster::raster(file) # Open GeoTIFF
  } else stop("No input given as 'file' or 'raster'.")
  
  # Convert lon/lat to "SpatialPoints"
  coords <- data.frame(lon=lon, lat=lat)
  coords <- sp::SpatialPoints(coords, proj4string = CRS("+init=epsg:4326")) # WGS-84
  
  # Transform to CRS of raster
  coords <- spTransform(coords, crs(gtiff))
  
  # Extract data for each location
  value <- raster::extract(gtiff, coords)
  
  return(value) # Output
}

#' Download SRTM data using raster package for bounding box of data.
#' 
#' SRTM data is automatically downloaded and, in case of more than one SRTM tile, 
#' merged together as a mosaic.
#' The SRTM RasterLayer Object can be cropped to the geographical extent of 
#' the data (crop = TRUE).
#' SRTM source: https://srtm.csi.cgiar.org/
#'
#' @param data data.table/data.frame with at least columns 'lon' and 'lat' 
#' @param directory directory path to optionally store the SRTM data 
#' @param outfile file path to save the SRTM raster as geotiff 
#' @param overwrite overwrite existing geotiff? Default is TRUE. 
#' @param crop crop raster/geotiff to data extent? Default is FALSE. 
#' @param ... additional parameters supported by raster::getData
#'
#' @return RasterLayer object with SRTM data
cqcp_download_srtm <- function(data, directory = NULL, outfile = NULL, 
                               overwrite = TRUE, crop = FALSE, ...){
  
  # Check if directory exists
  if(!is.null(directory)) {
    if(!dir.exists(directory)) dir.create(directory)
  }
  
  # Get bounding box coordinates
  min_lon <- min(data$lon)
  max_lon <- max(data$lon)
  min_lat <- min(data$lat)
  max_lat <- max(data$lat)
  
  # Download data
  ll <- raster::getData(name="SRTM", lat=min_lat, lon=min_lon, path = directory, ...)
  lr <- raster::getData(name="SRTM", lat=min_lat, lon=max_lon, path = directory, ...)
  ur <- raster::getData(name="SRTM", lat=max_lat, lon=max_lon, path = directory, ...)
  ul <- raster::getData(name="SRTM", lat=max_lat, lon=min_lon, path = directory, ...)
  
  # Combine tiles, if necessary
  mosaic <- raster::mosaic(ll, lr, ur, ul, fun=mean)
  
  # Crop to data extent
  if(crop) {
    data_extent <- as(raster::extent(min_lon, max_lon, min_lat, max_lat), 'SpatialPolygons')
    raster::crs(data_extent) <- raster::crs(mosaic)
    mosaic <- raster::crop(mosaic, data_extent)
  }
  
  # Store merged raster?
  if(!is.null(outfile)) {
    if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile))
    raster::writeRaster(mosaic, filename=outfile, overwrite=overwrite)
  }
  
  return(mosaic)
}

#' Add DEM height to data.table.
#' 
#' Adds Digital Elevation Model (DEM) information from a RasterLayer Object or 
#' geotiff to the data.table.
#' If no RasterLayer Object or file are given as parameters, SRTM data is 
#' automatically downloaded and used to extract DEM information at the positions
#' of the stations.
#' A new column 'z' is added to the data.table to be used in filter level M2.
#'
#' @param data data table/frame with at least columns 'lon' and 'lat' 
#' @param file path to a DEM geotiff 
#' @param raster RasterLayer object with DEM data 
#' @param directory directory path to optionally store downloaded SRTM data 
#' @param outfile file path to save the created SRTM raster as geotiff 
#' @param overwrite overwrite existing geotiff? 
#' @param crop crop SRTM raster/geotiff to data extent
#' @param na_vals set NA values in DEM to this value to avoid missing value in m2. Default: 0
#' @param ... additional parameters supported by raster::getData
#'
#' @return data table with new column 'z' with DEM information
cqcp_add_dem_height <- function(data, file = NULL, raster = NULL, 
                                directory = NULL, outfile = NULL, 
                                overwrite = TRUE, crop = FALSE, 
                                na_vals = 0, ...){
  
  # Extract unique locations from data
  locations <- data[, .SD[1], by = p_id, .SDcols = c("lon", "lat")]
  
  # Download SRTM data in case no file or raster is given
  if(is.null(raster) & is.null(file)){
    raster <- cqcp_download_srtm(locations, directory = directory, 
                                 outfile = outfile, overwrite = overwrite, 
                                 crop = crop, ...)
  }
  
  # Get height data per site
  height <- cqcp_extract_raster_data(locations$lon, locations$lat, 
                                     raster = raster, file = file)
  
  # deal with NA values
  height[is.na(height)] <- na_vals
  
  # Put information back together
  locations <- cbind(locations, z = height)
  data_out <- merge(data, locations[,.(p_id, z)], all.x = T, by = "p_id", sort = FALSE)
  
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
#' @param resolution temporal resolution as supported by lubridate. Default: '1 hour'
#' @param rounding_method Method to apply to round the time values to (cf. lubridate). 
#'   Default is 'nearest', to assign the values to the nearest full time value, 
#'   defined by 'resolution'. Other options are 'ceiling'/'ceil' and 'floor'.
#'
#' @return data.table with regular time series for all stations.
cqcp_padding <- function(data, resolution = "1 hour", rounding_method = "nearest") {
  
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
    print(paste0("Rounding method '",rounding_method,"' not supported. See documentation."))
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
  in_data <- in_data[, `:=`(ta, mean(ta, na.rm = T)),
                     by = .(p_id, time)][,.SD[1], by = .(p_id, time)]
  
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
