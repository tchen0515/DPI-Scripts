library(openxlsx)
library(geosphere) # for distance calculations
#library(PlanklinePS)

bin = function(x, y, xout, func = function (x) {mean(x, na.rm = T)}) {
  yout = rep(NA, length(xout))
  
  for (i in 1:length(xout)) {
    l = x == xout[i]
    if (sum(l) > 0) {
      yout[i] = func(y[l])
    }
  }
  
  yout
}

## Setup
save.dir = paste0(root.dir, '/_rdata/')
#root.dir is defined in Sensor A.R<-F:/DPI/DPI-sensor/P2402-sensor/sensor_data/cycle1_day1/
# F:/DPI/DPI-sensor/P2402-sensor/sensor_data/cycle1_day1/

gps = readRDS(paste0(save.dir, 'gps.rds'))

# Ensure gps remains a data frame even if only one column matches
valid_cols = intersect(c("Time", "Latitude", "Longitude", "latitude", "longitude", "lat", "lon"), names(gps))
gps = gps[, valid_cols, drop = FALSE]

# Dynamically rename spatial columns to standard capitalization if present
names(gps)[grep("^lat", names(gps), ignore.case = TRUE)] = "Latitude"
names(gps)[grep("^lon", names(gps), ignore.case = TRUE)] = "Longitude"

# Guarantee gps is a data.frame
gps = as.data.frame(gps)

ctd1 = readRDS(paste0(save.dir, 'ctd1.rds'))
ctd2 = readRDS(paste0(save.dir, 'ctd2.rds'))
eng = readRDS(paste0(save.dir, 'engineering.rds'))
fluoro1 = readRDS(paste0(save.dir, 'fluorometer1.rds'))
fluoro2 = readRDS(paste0(save.dir, 'fluorometer2.rds'))
analog = readRDS(paste0(save.dir, 'analog.rds'))
acs = readRDS(paste0(save.dir, 'acs.rds'))
suna = readRDS(paste0(save.dir, 'suna.rds'))

## bin time

dt = 2 #seconds

gps$Time = as.POSIXct(round(as.numeric(gps$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
ctd1$Time = as.POSIXct(round(as.numeric(ctd1$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
ctd2$Time = as.POSIXct(round(as.numeric(ctd2$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
eng$Time = as.POSIXct(round(as.numeric(eng$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
fluoro1$Time = as.POSIXct(round(as.numeric(fluoro1$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
fluoro2$Time = as.POSIXct(round(as.numeric(fluoro2$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
analog$Time = as.POSIXct(round(as.numeric(analog$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
acs$Time = as.POSIXct(round(as.numeric(acs$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")
suna$Time = as.POSIXct(round(as.numeric(suna$Time) / dt) * dt, origin = "1970-01-01", tz = "UTC")

## Robust interpolation helper (filters NA pairs and prevents length crashes)
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

## Merging
dpi = data.frame(Time = unique(gps$Time))


## GPS
for (n in names(gps)[-1]) {
  dpi[[n]] = safe_approx(gps$Time, gps[[n]], dpi$Time, ties = mean)
}

## CTD
for (n in names(ctd1)[-1]) {
  dpi[[n]] = safe_approx(ctd1$Time, ctd1[[n]], dpi$Time, ties = mean)
}
for (n in names(ctd2)[-1]) {
  dpi[[n]] = safe_approx(ctd2$Time, ctd2[[n]], dpi$Time, ties = mean)
}

## Analog
for (n in names(analog)[2:5]) {
  dpi[[n]] = safe_approx(analog$Time, analog[[n]], dpi$Time, ties = mean)
}

##Fluorometer
for (n in names(fluoro1)[c(3,5,7)]) {
  dpi[[n]] = safe_approx(fluoro1$Time, fluoro1[[n]], dpi$Time, ties = median)
}
for (n in names(fluoro2)[c(3,5,7)]) {
  dpi[[n]] = safe_approx(fluoro2$Time, fluoro2[[n]], dpi$Time, ties = median)
}

## ENG
for (n in names(eng)[2:6]) {
  dpi[[n]] = safe_approx(eng$Time, eng[[n]], dpi$Time, ties = mean)
}

## ACs
dpi[['a440']] = safe_approx(acs$Time, acs$A440, dpi$Time, ties = median)
dpi[['a675']] = safe_approx(acs$Time, acs$A675.8, dpi$Time, ties = median)

## SUNA
for (n in names(suna)[-1]) {
  dpi[[n]] = safe_approx(suna$Time, suna[[n]], dpi$Time, ties = median)
}


#### Filter
dpi = dpi[dpi$Pressure > 2,]

# Break at time gaps
k = c(0, which(diff(dpi$Time) > 3600), nrow(dpi))

transects = list()

# Dynamically identify longitude and latitude column names (case-insensitive)
lon_col = names(dpi)[grep("lon", names(dpi), ignore.case = TRUE)][1]
lat_col = names(dpi)[grep("lat", names(dpi), ignore.case = TRUE)][1]

for (i in 2:length(k)) {
  sub_df = dpi[(k[i-1]+1):k[i],]  
  n_rows = nrow(sub_df)
  
  if (n_rows > 1 && !is.na(lon_col) && !is.na(lat_col)) {
    # Extract coordinate matrices safely
    p1 = as.matrix(sub_df[1:(n_rows - 1), c(lon_col, lat_col)])
    p2 = as.matrix(sub_df[2:n_rows, c(lon_col, lat_col)])
    
    # Identify row steps where BOTH start (p1) and end (p2) points have valid coordinates
    valid_pairs = !is.na(p1[, 1]) & !is.na(p1[, 2]) & !is.na(p2[, 1]) & !is.na(p2[, 2])
    
    step_dists = numeric(n_rows - 1) # Initialize step distances as 0
    
    if (any(valid_pairs)) {
      # Pass ONLY non-NA coordinate rows to distCosine
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