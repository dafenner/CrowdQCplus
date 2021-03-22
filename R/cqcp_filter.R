#' Main QC step m1
#'
#' Flag values with FALSE that are missing or belong to a p_id having lon & lat
#' values which occur more often than the 'cutOff' value.
#'
#' @param data data set (a data.table object) formated the same way as the sample data (netatmoBer)
#' @param cutOff how much stations are allowed to have the same coordinates,
#'   default is 1. This means that if two p_ids share the same lon & lat values,
#'   data at these stations are set to FALSE.
#'
#' @return data.table
#' @export
#'
#' @examples
#' data(netatmoBer)
#' x <- m1(netatmoBer)
cqcp_m1 <- function(data, cutOff = 1){
  val  <- data[!is.na(ta),.(a = 1), by = .(p_id,lon,lat)]
  bad_s  <- val[,.(anz = sum(lon == val$lon & lat == val$lat)), by = p_id]
  bad_s  <- bad_s[anz > cutOff,]$p_id
  data[,m1:= T]
  data[p_id %in% bad_s,"m1"] <- F
  data[is.na(ta),"m1"] <- F
  return(data)
}

#' Version of the Qn function respecting NaN values
#'
#' @param x
#'
#' @return NaN for no valid values, otherwise robust scale estimator 'Qn' without NaN
cqcp_Qnr <- function(x){
  x <- x[!is.na(x)]
  return(robustbase::Qn(x))
}

#' Calculate robust z-score
#'
#' @param x vector to calculate the robust z-score from
#'
#' @return vector with z-score for elements in x or NaN
cqcp_getZ <- function(x){
  q <- cqcp_Qnr(x)
  if(is.na(q)){
    return(NaN)
  }
  res <- (x / q) - (median(x, na.rm = T)/q)
  return(res)
}

#' Main QC step m2
#'
#' Flags all values as FALSE if robust z-score is not within the critical values
#' obtained from low and high. This approach is based on the distribution
#' function of all values. !Careful: if number of stations is < 200, it would be
#' better to use the Student t distribution (to be implemented)!
#'
#' @param data data.table object obtained from m1
#' @param low 0 < low < high < 1
#' @param high 0 < low < high < 1
#' @param heightCorrection if set to true (default) and the column "z" exists in
#'   the input data, the temperatures used in calculating the z-score are
#'   corrected. The applied formula is ta_cor = ta + ((lapse_rate * (z - mz)) 
#'   where mz is the spatial mean of z at each time step.
#' @param lapse_rate Lapse rate to use in 'heightCorrection'. Default is the 
#'   environmental lapse rate of -0.0065 K/m. Set as a positive value: e.g.
#'   lapse_rate = 0.01 to set a dry adiabatic lapse rate.
#' @param debug set to true to keep intermediate results
#'
#' @return data.table
#' @export
#'
#' @examples
#' data(netatmoBer)
#' x <- m1(netatmoBer)
#' y <- m2(x)
#'
cqcp_m2 <- function(data, low = 0.01, high = 0.95, heightCorrection = T, debug = F,
               lapse_rate = 0.0065, fun = qnorm){
  data[,rem_ta := ta]
  # ensures that all what is wrong in m1 is wrong in m2 too
  data[!m1, "rem_ta"] <- NaN
  if(heightCorrection & "z" %in% colnames(data)){
    agg <- data[,.(mz = mean(z, na.rm = T)), by=.(time)]
    data <- merge(data,agg, by = "time")
    data[, rem_ta := rem_ta + (lapse_rate * (z - mz))]
  }
  data[, z_ta := cqcp_getZ(rem_ta), by = time]
  data[, m2 := T]
  data[z_ta < fun(low) | z_ta > fun(high) | is.nan(z_ta), "m2"] <- F
  if(!debug){
    data$rem_ta <- NULL
    data$z_ta <- NULL
    if(heightCorrection & "z" %in% colnames(data)){
      data$mz <- NULL
    }
  }
  return(data)
}

#' add_episode
#'
#' @param data data.table with at least columns 'p_id' and 'time'
#' @param column 
#'
#' @return data.table with additional column 'episode'
cqcp_add_episode <- function(data, duration){
  # works with duration from lubridate since regular time series
  steps <- lubridate::duration(duration)/(data[2]$time-data[1]$time)
  n <- data[p_id == data[1]$p_id, .N]
  episode <- rep(1:ceiling(n/steps), each = steps)
  if(length(episode) != n) {
    print("Last episode shorter than specified duration.")
  }
  data <- data[, episode := episode[1:n], by = .(p_id)]
  return(data)
}

#' Monthly correlation of individual CWS with median
#'
#' Calculates the correlation of each CWS vs. a data.set containing an
#' aggregated time series in the column 'med' per month. Both series need to
#' have the same length and values at the same position are expected to belong
#' to the same position in time. Function is called internally by QC m4.
#'
#' @param x values of unaggregated time series
#' @param y data.table containing column 'med' holding aggregated values and
#'   month holding the month the time series belongs to
#' @param m month to base the calculation on
#' @param cutOff value below which FALSE is returned.
#'
#' @return TRUE if correlation for the given month is higher than cutOff, FALSE
#'   otherwise
#'
#' @examples
#' see m4
cqcp_cor_month <- function(x, y, m, cutOff){
  if(length(x) != length(y[month == m,]$med)){
    stop("Dimensions are off, are you sure your data set contain an NaN value for each p_id at each missing time step?")
  }
  c <- suppressWarnings(cor(x, y[month == m,]$med, use="pairwise.complete.obs")) #suppress warning if no pairwise complete obs exists, just return FALSE
  if(is.na(c)){
    return(F) #treat NA as no correlation
  }
  if(c < cutOff){
    return(F)
  }
  return(T)
}

#' Main QC step m3
#'
#' Flag values with FALSE if more than cutOff percent values are removed during
#' m2 per month. This is done since it is assumed that if too many individual
#' values are flagged FALSE in m2, the station is too suspicious to be kept.
#'
#' @param data data.table object obtained from m2
#' @param cutOff value above which data are flagged with FALSE, 0 < cutOff < 1.
#'   Default is 0.2, i.e., 20 percent of data.
#'
#' @return data.table
#' @export
#'
#' @examples
#' y <- m2(x)
#' z <- m3(y)
cqcp_m3 <- function(data, cutOff = 0.2, complete = FALSE, duration = NULL, 
                    rolling = FALSE){
  
  # case 1: nothing specified (rolling ignored, bec. duration has to be set).
  # as in original CrowdQC: per month
  if(is.null(duration) & !complete) {
    has_m <- cqcp_has_column(data, column = "month")
    if(!has_m){
      data[,month := lubridate::floor_date(time,"month")]
    }
    data[, m3 := m2 & sum(m2)/.N > cutOff, by = .(month, p_id)]
    if(!has_m){
      data$month <- NULL
    }
  } else if(complete) { # complete time series, 'duration' and 'rolling' ignored
    data[, m3 := m2 & sum(m2)/.N > cutOff, by = .(p_id)]
  } else { # per duration
    if(rolling) { # rolling application
      if(!is.null(duration)) {
      print("Rolling application not yet possible.") # not yet implemented
      }
    } else { # per duration, not rolling
      has_e <- cqcp_has_column(data, column = "episode")
      if(!has_e){
        data <- cqcp_add_episode(data, duration)
      }
      data[, m3 := m2 & sum(m2)/.N > cutOff, by = .(episode, p_id)]
      if(!has_e){
        data$episode <- NULL
      }
    }
  }
  return(data)
}

#' Main QC step m4
#'
#' Flag values with FALSE if they belong to a month in which the correlation
#' with the median of all stations is lower than cutOff.
#'
#' @param data data.table as returned by m3
#' @param cutOff value of correlation coefficient below which data are flagged
#'   with FALSE, 0 < cutOff < 1. Default is 0.9.
#'
#' @return data.table
#' @export
#'
#' @examples
#' y <- m3(x)
#' z <- m4(y)
cqcp_m4 <- function(data, cutOff = 0.9, complete = FALSE, duration = NULL, 
                    rolling = FALSE){
  
  data[,rem_ta := ta]
  data[!m3, "rem_ta"] <- NaN
  
  # case 1: nothing specified (rolling ignored, bec. duration has to be set).
  # as in original CrowdQC: per month
  if(is.null(duration) & !complete) {
    has_m <- cqcp_has_column(data, column = "month")
    if(!has_m){
      data[,month := lubridate::floor_date(time,"month")]
    }
    data_agg <- data[,.(med = median(rem_ta, na.rm = T)), by=.(month, time)]
    cor_station <- data[,.(c = cqcp_cor_month(rem_ta, data_agg, unique(month), cutOff)), by = .(month, p_id)]
    data <- merge(data, cor_station, all.x = T, by = c("month", "p_id"))
    if(!has_m){
      data$month <- NULL
    }
  } else if(complete) { # complete time series, 'duration' and 'rolling' ignored
    data_agg <- data[,.(med = median(rem_ta, na.rm = T)), by=.(time)]
    cor_station <- data[,.(c = cqcp_cor_month(rem_ta, data_agg, unique(month), cutOff)), by = .(p_id)]
    data <- merge(data, cor_station, all.x = T, by = "p_id")
  } else { # per duration
    if(rolling) { # rolling application
      if(!is.null(duration)) {
        print("Rolling application not yet possible.") # not yet implemented
      }
    } else { # per duration, not rolling
      has_e <- cqcp_has_column(data, column = "episode")
      if(!has_e){
        data <- cqcp_add_episode(data, duration)
      }
      data_agg <- data[,.(med = median(rem_ta, na.rm = T)), by=.(episode, time)]
      cor_station <- data[,.(c = cqcp_cor_month(rem_ta, data_agg, unique(episode), cutOff)), by = .(episode, p_id)]
      data <- merge(data, cor_station, all.x = T, by = c("episode", "p_id"))
      if(!has_e){
        data$episode <- NULL
      }
    }
  }
  data[, m4 := c & m3]
  data$c <- NULL
  data$rem_ta <- NULL
  return(data)
}

#' Interpolation
#'
#' This function takes a numerical vector x and fills NaNs with linearly
#' interpolated values. The allowed length of the gap, i.e., the number of
#' consecutive NaNs to be interpolated and replaced, is smaller or equal
#' maxLength. Internally called by o1.
#'
#' @param x a numeric vector
#' @param maxLength allowed length of the gap to interpolate, default is 1.
#'
#' @return vector
#' @export
#'
#' @examples
#' x <- x(1, NaN, 3, NaN NaN, 6, NaN, 8)
#' interpol(x)
#' interpol(x, 2)
cqcp_interpol <- function(x, maxLength = 1){
  rl <- rle(is.na(x))
  re <- rl$lengths <= maxLength & rl$values
  idx <- rep(re,rl$lengths)
  if(sum(!is.na(x)) < 2){
    return(x)
  }
  x[idx] <- approx(1:length(x), x, (1:length(x))[idx])$y
  return(x)
}

#' Optional QC step o1
#'
#' In this step missing data is interpolated, default is to perform linear
#' interpolation on gaps of maximal length = 1.
#' A new column named "ta_int" is added to the data.table, containing the
#' filtered data at level m4 with the interpolated data.
#'
#' @param data data.table as returned from m4
#' @param fun  function to use for interpolation, default is interpol
#' @param ...  additional parameters for interpolation function
#'
#' @return data.table
#' @export
#'
#' @examples
#' #default
#' o_1 <- o1(m_4)
#' #interpolate gaps up to 5 hours
#' o_1 <- o1(m_4, maxLength = 5)
cqcp_o1 <- function(data, fun = cqcp_interpol, ...){
  data[,ta_int := ta]
  data[!m4, "ta_int"] <- NA
  data[,ta_int := fun(ta_int, ...), by = .(p_id)]
  is_interpolated = data$ta != data$ta_int
  #handle NA compare
  #first NA's are always false
  is_interpolated[is.na(is_interpolated)] <- FALSE
  #but if we created a value it has to be interpolated
  is_interpolated[is.na(data$ta) & !is.na(data$ta_int)] <- TRUE
  data[, o1:= is_interpolated | m4] #a value ready to use
  return(data)
}

#' Optional QC step o2
#'
#' Optional QC for temporal data availability. Flags all values in a
#' calendar day as FALSE if less than 'cutOff' percent of valid values are
#' available for that day.
#'
#' @param data data.table as returned from o1
#' @param cutOff percentage of values that must be present for each day before
#'   all values of that day are flagged with FALSE, expressed in fraction: 0 <
#'   cutOff < 1. Default is 0.8, i.e, 80 percent of data.
#'
#' @return data.table
#' @export
#'
#' @examples
#' o_2 <- o2(o_1)
cqcp_o2 <- function(data, cutOff = 0.8){
  has_d <- "day" %in% colnames(data)
  if(!has_d){
    data[, day := lubridate::floor_date(time,"day")]
  }
  data[, o2 := o1 & sum(o1)/.N > cutOff, by = .(day, p_id)]
  if(!has_d){
    data$day <- NULL
  }
  return(data)
}

#' Optional QC step o3
#'
#' Optional QC for temporal data availability. Flags all values in a month
#' as FALSE if less than 'cutOff' percent of valid values are available for that
#' month.
#'
#' @param data data.table as returned from o2
#' @param cutOff percentage of values that must be present for each month before
#'   all values of that month are flagged with FALSE, expressed in fraction: 0 <
#'   cutOff < 1. Default is 0.8, i.e, 80 percent of data.
#'
#' @return data.table
#' @export
#'
#' @examples
#' o_3 <- o3(o_2)
cqcp_o3 <- function(data, cutOff = 0.8){
  has_m <- cqcp_has_column(data, column = "month")
  if(!has_m){
    data[, month := lubridate::floor_date(time,"month")]
  }
  data[, o3 := o2 & sum(o2)/.N > cutOff, by = .(month, p_id)]
  if(!has_m){
    data$month <- NULL
  }
  return(data)
}

#' has_column
#'
#' @param data
#' @param column The column to look for. Default: "month"
#'
#' @return true if data contains a specific column
cqcp_has_column <- function(data, column = "month"){
  return(column %in% colnames(data))
}


#' Complete quality control (QC) of CWS data
#'
#' Performs all QC steps in consecutive order. All settings are according
#' to Napoly et al. (2018). This is the default function to carry out the
#' complete QC procedure. Each QC step takes the result of the previous
#' QC step as input. Thus, e.g., when applying QC step m2, the column
#' 'm1' must be in the input data, for QC step m3 the column 'm2' must be
#' present, etc. Each individual step could be skipped by renaming the columns
#' in the input data. After each QC step a new column of type BYTE is
#' included in the output, containing TRUE or FALSE flags. Flags of the previous
#' levels are carried along, i.e., if a value failed in step m2, this FALSE is
#' kept throughout the remaining QC steps. In the end, only those data
#' values containing TRUE after the all QC steps are valid according to this
#' QC.
#'
#' @param data input data in the format as the example data (netatmoBer)
#' @param m1_cutOff see cutOff in ?m1
#' @param m2_low see low in ?m2
#' @param m2_high see high in ?m2
#' @param m3_cutOff see cutOff in ?m3
#' @param m4_cutOff see cutOff in ?m4
#' @param o1_fun see fun in ?o1
#' @param o2_cutOff see cutOff in ?o2
#' @param o3_cutOff see cutOff in ?o3
#' @param includeOptional set to TRUE if QC steps o1 to o3 shall be
#'   performed, default: TRUE
#' @param ... additional parameters used in o1. For details see ?o1
#'
#' @return data.table
#' @export
#'
#' @examples
#' data(netatmoBer)
#' y <- qcCWS(netatmoBer)
cqcp_qcCWS <- function(data,
                  m1_cutOff = 1,
                  m2_low = 0.1, m2_high = 0.95, m2_lapse_rate = 0.006, m2_fun = qnorm, 
                  m3_cutOff = 0.2,
                  m4_cutOff = 0.9,
                  o1_fun = interpol,
                  o2_cutOff = 0.8,
                  o3_cutOff = 0.8,
                  includeOptional = T,
                  ...){
  data[, month := lubridate::floor_date(time,"month")]
  data <- cqcp_m1(data, cutOff = m1_cutOff)
  data <- cqcp_m2(data, low = m2_low, high = m2_high, lapse_rate = m2_lapse_rate, 
             fun = m2_fun)
  data <- cqcp_m3(data, cutOff = m3_cutOff)
  data <- cqcp_m4(data, cutOff = m4_cutOff)
  if(includeOptional){
    data <- cqcp_o1(data, fun = o1_fun, ...)
    data <- cqcp_o2(data, cutOff = o2_cutOff)
    data <- cqcp_o3(data, cutOff = o3_cutOff)
  }
  data$month <- NULL
  return(data)
}
