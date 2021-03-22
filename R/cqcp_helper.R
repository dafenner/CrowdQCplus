
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
