library(openxlsx)
library(geosphere)
library(dplyr)

## Setup
save.dir = paste0(root.dir, '/_rdata/')

gps     = readRDS(paste0(save.dir, 'gps.rds'))
ctd1    = readRDS(paste0(save.dir, 'ctd1.rds'))
ctd2    = readRDS(paste0(save.dir, 'ctd2.rds'))
eng     = readRDS(paste0(save.dir, 'engineering.rds'))
fluoro1 = readRDS(paste0(save.dir, 'fluorometer1.rds'))
fluoro2 = readRDS(paste0(save.dir, 'fluorometer2.rds'))
analog  = readRDS(paste0(save.dir, 'analog.rds'))
acs     = readRDS(paste0(save.dir, 'acs.rds'))
suna    = readRDS(paste0(save.dir, 'suna.rds'))

# Ensure GPS dataframe & standard column names
valid_cols = intersect(c("Time", "Latitude", "Longitude", "latitude", "longitude", "lat", "lon"), names(gps))
gps = as.data.frame(gps[, valid_cols, drop = FALSE])
names(gps)[grep("^lat", names(gps), ignore.case = TRUE)] = "Latitude"
names(gps)[grep("^lon", names(gps), ignore.case = TRUE)] = "Longitude"

## -----------------------------------------------------------------------------
## SAFE TIME ALIGNMENT HELPER (Handles both Character and POSIXct safely)
## -----------------------------------------------------------------------------
align_time = function(df, dt = 2) {
  if (is.null(df) || !("Time" %in% names(df)) || nrow(df) == 0) return(df)
  
  # Ensure Time is POSIXct UTC before calling as.numeric
  if (!inherits(df$Time, "POSIXt")) {
    df$Time = as.POSIXct(df$Time, tz = "UTC")
  }
  
  # Round to bin width (dt)
  df$Time = as.POSIXct(round(as.numeric(df$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
  return(df)
}

dt = 2 # bin width in seconds

gps     = align_time(gps, dt)
ctd1    = align_time(ctd1, dt)
ctd2    = align_time(ctd2, dt)
eng     = align_time(eng, dt)
fluoro1 = align_time(fluoro1, dt)
fluoro2 = align_time(fluoro2, dt)
analog  = align_time(analog, dt)
acs     = align_time(acs, dt)
suna    = align_time(suna, dt)

## Robust interpolation helper
safe_approx = function(x, y, xout, ties = mean) {
  x_num = suppressWarnings(as.numeric(x))
  y_num = suppressWarnings(as.numeric(y))
  valid = !is.na(x_num) & !is.na(y_num)
  
  if (sum(valid) >= 2) {
    approx(x = x_num[valid], y = y_num[valid], xout = as.numeric(xout), ties = ties)$y
  } else {
    rep(NA, length(xout))
  }
}

## -----------------------------------------------------------------------------
## MERGING INTO MASTER DPI DATAFRAME
## -----------------------------------------------------------------------------
# Initialize master timeline from valid GPS timestamps
gps_clean_time = gps$Time[!is.na(gps$Time)]
dpi = data.frame(Time = unique(gps_clean_time))

# Helper function to safely merge features WITHOUT overwriting dpi$Time
merge_sensor_cols = function(dpi_df, sensor_df, cols_to_merge, ties_func = mean) {
  if (is.null(sensor_df) || nrow(sensor_df) == 0) return(dpi_df)
  
  # Strictly exclude "Time" so dpi$Time can NEVER be overwritten
  cols_to_merge = setdiff(cols_to_merge, "Time")
  
  for (col in cols_to_merge) {
    if (col %in% names(sensor_df)) {
      dpi_df[[col]] = safe_approx(sensor_df$Time, sensor_df[[col]], dpi_df$Time, ties = ties_func)
    }
  }
  return(dpi_df)
}

## GPS
dpi = merge_sensor_cols(dpi, gps, names(gps), ties_func = mean)

## CTD
dpi = merge_sensor_cols(dpi, ctd1, names(ctd1), ties_func = mean)
dpi = merge_sensor_cols(dpi, ctd2, names(ctd2), ties_func = mean)

## Analog
analog_cols = intersect(names(analog), names(analog)[2:min(5, ncol(analog))])
dpi = merge_sensor_cols(dpi, analog, analog_cols, ties_func = mean)

## Fluorometer
fluoro1_cols = intersect(names(fluoro1), names(fluoro1)[c(3, 5, 7)])
dpi = merge_sensor_cols(dpi, fluoro1, fluoro1_cols, ties_func = median)

fluoro2_cols = intersect(names(fluoro2), names(fluoro2)[c(3, 5, 7)])
dpi = merge_sensor_cols(dpi, fluoro2, fluoro2_cols, ties_func = median)

## ENG
eng_cols = intersect(names(eng), names(eng)[2:min(6, ncol(eng))])
dpi = merge_sensor_cols(dpi, eng, eng_cols, ties_func = mean)

## ACs (Dynamic case-insensitive column matching)
if (!is.null(acs) && nrow(acs) > 0) {
  a440_col = names(acs)[grep("a440", names(acs), ignore.case = TRUE)][1]
  a675_col = names(acs)[grep("a675", names(acs), ignore.case = TRUE)][1]
  
  if (!is.na(a440_col)) {
    dpi[['a440']] = safe_approx(acs$Time, acs[[a440_col]], dpi$Time, ties = median)
  }
  if (!is.na(a675_col)) {
    dpi[['a675']] = safe_approx(acs$Time, acs[[a675_col]], dpi$Time, ties = median)
  }
}

## SUNA
dpi = merge_sensor_cols(dpi, suna, names(suna), ties_func = median)

#### Filter
if ("Pressure" %in% names(dpi)) {
  dpi = dpi[!is.na(dpi$Pressure) & dpi$Pressure > 2, ]
}

# Break at time gaps (> 1 hour)
if (nrow(dpi) > 0) {
  k = c(0, which(diff(dpi$Time) > 3600), nrow(dpi))
  
  transects = list()
  
  lon_col = names(dpi)[grep("lon", names(dpi), ignore.case = TRUE)][1]
  lat_col = names(dpi)[grep("lat", names(dpi), ignore.case = TRUE)][1]
  
  for (i in 2:length(k)) {
    sub_df = dpi[(k[i-1]+1):k[i], ]  
    n_rows = nrow(sub_df)
    
    if (n_rows > 1 && !is.na(lon_col) && !is.na(lat_col)) {
      p1 = as.matrix(sub_df[1:(n_rows - 1), c(lon_col, lat_col)])
      p2 = as.matrix(sub_df[2:n_rows, c(lon_col, lat_col)])
      
      valid_pairs = !is.na(p1[, 1]) & !is.na(p1[, 2]) & !is.na(p2[, 1]) & !is.na(p2[, 2])
      step_dists = numeric(n_rows - 1)
      
      if (any(valid_pairs)) {
        step_dists[valid_pairs] = geosphere::distCosine(
          p1[valid_pairs, , drop = FALSE], 
          p2[valid_pairs, , drop = FALSE]
        ) / 1e3
      }
      
      sub_df$Distance = cumsum(c(0, step_dists))
    } else if (n_rows >= 1) {
      sub_df$Distance = 0
    } else {
      sub_df$Distance = numeric(0)
    }
    
    transects[[paste0('dpi', i-1)]] = sub_df
  }
  
  saveRDS(transects, file = paste0(save.dir, 'transects.rds'))
  saveRDS(dpi, file = paste0(save.dir, 'dpi.rds'))
  cat("Successfully merged sensors and generated dpi.rds and transects.rds!\n")
} else {
  warning("dpi dataframe is empty after filtering!")
}