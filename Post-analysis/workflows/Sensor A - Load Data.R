### plot maps & env profiles (Section plot)

## -----------------------------------------------------------------------------
## SETUP & CONFIGURATION (Only change dataset_name when switching datasets)
## -----------------------------------------------------------------------------
dataset_name = "cycle3_day1"
root.dir = 'F:/DPI/DPI-sensor/P2402-sensor/sensor_data/cycle3_day1/'

#library(devtools)
#devtools::install_github("tbrycekelly/TheSource", dependencies = FALSE) # The package's repo
#library(TheSource)
library(oce)   # alternation of TheSource
library(openxlsx)
#library(PlanklinePS)
## Source all sensor input function scripts
source('F:/GithubDesktop/DPI-Scripts/Post-analysis/processing/load.dpi.files.R')
source('F:/GithubDesktop/DPI-Scripts/Post-analysis/3rd party/load.acs.files.R')
source('F:/GithubDesktop/DPI-Scripts/Post-analysis/3rd party/load.lisst.files.R')
source('F:/GithubDesktop/DPI-Scripts/Post-analysis/3rd party/load.suna.files.R')

# Helper function to replace make_time_utc()
make_time_utc <- function(year) as.POSIXct(paste0(year, "-01-01"), tz = "UTC")

## Source all function scripts in a specific folder
function_dir <- "F:/DPI/GithubDesktop/Post-analysis/processing/"
function_files <- list.files(path = function_dir, pattern = "\\.R$", full.names = TRUE)

for (file in function_files) {
  tryCatch({
    source(file)
  }, error = function(e) {
    message("Skipping script with top-level code: ", basename(file))
  })
}

## Default directories
directories = list(
  root = root.dir,
  gps = paste0(root.dir, 'GPS Publisher/'),
  ctd1 = paste0(root.dir, 'CTD 1 Publisher/'),
  ctd2 = paste0(root.dir, 'CTD 2 Publisher/'),
  ad = paste0(root.dir, 'AD1216 Publisher/'),
  fl1 = paste0(root.dir, 'Fluorometer 1 Publisher/'),
  fl2 = paste0(root.dir, 'Fluorometer 2 Publisher/'),
  eng = paste0(root.dir, 'Inclinometer Publisher/'),
  lisst = paste0(root.dir, 'LISST/'),
  acs = paste0(root.dir, 'ACS/'),
  suna = paste0(root.dir, 'SUNA/'),
  save = paste0(root.dir, '/_rdata/')
)

if (!dir.exists(directories$root)) {stop('Root directory not found. Aborting!')}

for (i in 2:length(directories)) {
  if (!dir.exists(directories[[i]])) {
    message('Creating missing directory: ', directories[[i]])
    dir.create(directories[[i]])
  }
}

### GPS Files
gps.files = list.files(path = directories$gps,
                       full.names = T,
                       recursive = T,
                       pattern = '*.csv')
gps = load.dpi.files(gps.files)

## Assign names to all relevant columns
colnames(gps)[1:7] = c('Time', 'Message', 'datetime', 'Latitude', 'Hemisphere', 'Longitude', 'Hemi_Lon')

## Force coordinates to numeric
gps$Latitude  = as.numeric(as.character(gps$Latitude))
gps$Longitude = as.numeric(as.character(gps$Longitude))

## Convert NMEA DegMin.m format to Decimal Degrees
gps$Latitude  = floor(gps$Latitude / 100) + (gps$Latitude - floor(gps$Latitude / 100) * 100) / 60
gps$Longitude = -1 * (floor(gps$Longitude / 100) + (gps$Longitude - floor(gps$Longitude / 100) * 100) / 60)

## Keep ONLY Time, Latitude, and Longitude
gps = gps[, c('Time', 'Latitude', 'Longitude')]

## Save updated file
saveRDS(gps, file = paste0(directories$save, 'gps.rds'))



#### CTD 1
## Initial setup
ctd.files = list.files(directories$ctd1,
                       full.names = T,
                       pattern = '*.csv',
                       recursive = T)

ctd = load.dpi.files(ctd.files)

colnames(ctd) = c('Time', 'Temperature', 'Conductivity', 'Pressure', 'Depth', 'Salinity', 'Sound.Velocity')


## Calculate
ctd$SigmaTheta = oce::swSigmaTheta(salinity = ctd$Salinity, 
                                   temperature = ctd$Temperature, 
                                   pressure = ctd$Pressure)
ctd = ctd[ctd$Time > make_time_utc(2000),]
saveRDS(ctd, file = paste0(directories$save, 'ctd1.rds'))


#### CTD 2
## Initial setup
ctd.files = list.files(directories$ctd2,
                       full.names = T,
                       pattern = '*.csv',
                       recursive = T)

ctd = load.dpi.files(ctd.files)
ctd = ctd[ctd$Time > make_time_utc(2000),]

colnames(ctd) = c('Time', 'Temperature2', 'Conductivity2', 'Pressure2', 'Depth2', 'Salinity2', 'Sound.Velocity2')

## Calculate
ctd$SigmaTheta = oce::swSigmaTheta(salinity = ctd$Salinity, 
                                   temperature = ctd$Temperature, 
                                   pressure = ctd$Pressure)
saveRDS(ctd, file = paste0(directories$save, 'ctd2.rds'))



#### Analog
## Set up format
ad.files = list.files(directories$ad,
                      full.names = T,
                      pattern = '.csv',
                      recursive = T)

ad = load.dpi.files(ad.files)

colnames(ad) = c('Time', 'Oxygen', 'PAR', 'pH', 'REDOX', 'Checksum', 'checksum2')

ad = ad[ad$Time > make_time_utc(2000),]
saveRDS(ad, file = paste0(directories$save, 'analog.rds'))


#### Fluorometer 1
fl.files = list.files(directories$fl1,
                      full.names = T,
                      recursive = T, 
                      pattern = '*.csv')
fl = load.dpi.files(fl.files)

colnames(fl) = c('Time', 'Chl.wave', 'Chl', 'FDOm.wave', 'FDOM', 'Phycocyanin.wave', 'Phycocyanin')

## Filter
fl$Chl[fl$Chl > 4e3 | fl$Chl < 10] = NA
fl$FDOM[fl$FDOM > 4e3 | fl$FDOM < 10] = NA
fl$Phycocyanin[fl$Phycocyanin > 4e3 | fl$Phycocyanin < 10] = NA

## Convert counts to real units
# Calibration from ???
fl$Chl = (fl$Chl - 49) * 0.0121
fl$FDOM = (fl$FDOM - 50) * 0.0914
fl$Phycocyanin = (fl$Phycocyanin - 46) * 0.0422

fl = fl[fl$Time > make_time_utc(2000),]
saveRDS(fl, file = paste0(directories$save, 'fluorometer1.rds'))


#### Fluorometer 2
fl.files = list.files(directories$fl2,
                      full.names = T,
                      recursive = T,
                      pattern = '*.csv')
fl = load.dpi.files(fl.files)

colnames(fl) = c('Time', 'S470.wave', 'S470', 'S532.wave', 'S532', 'S650.wave', 'S650')

## Filter
fl$S470[fl$S470 > 4e3 | fl$S470 < 10] = NA
fl$S532[fl$S532 > 4e3 | fl$S532 < 10] = NA
fl$S650[fl$S650 > 4e3 | fl$S650 < 10] = NA

## Convert counts to real units
# Calibration from ???
fl$S470 = (fl$S470 - 35) * 0.00001181
fl$S532 = (fl$S532 - 50) * 0.000008216
fl$S650 = (fl$S650 - 50) * 0.00000411

fl = fl[fl$Time > make_time_utc(2000),]
saveRDS(fl, file = paste0(directories$save, 'fluorometer2.rds'))



#### Eng
eng.files = list.files(directories$eng,
                       full.names = T,
                       pattern = '*.csv',
                       recursive = T)

eng = load.dpi.files(eng.files)

colnames(eng) = c('Time', 'AccelerationX', 'AccelerationY', 'AccelerationZ', 'Roll', 'Pitch', 'T')
eng = eng[eng$Time > make_time_utc(2000),]
saveRDS(eng, file = paste0(directories$save, 'engineering.rds'))



#### LISST
# TODO: timestamps are not right!
lisst.files = list.files(directories$lisst,
                         full.names = T,
                         pattern = '*_rs.csv',
                         recursive = T)
lisst = load.lisst.files(lisst.files)

saveRDS(lisst, file = paste0(directories$save, 'lisst.rds'))



#### ACs
acs.files = list.files(directories$acs,
                       full.names = T,
                       pattern = '*.dat',
                       recursive = T)
acs = load.acs.files(acs.files)

acs = cbind(Time = acs$Time, acs[,which(colnames(acs) != 'Time')])
acs$V178 = NULL

saveRDS(acs, file = paste0(directories$save, 'acs.rds'))


#### SUNA
suna.files = list.files(directories$suna,
                        pattern = "\\.(csv|txt|dat)$", 
                        ignore.case = TRUE,
                        full.names = TRUE)

# 1. Separate CSV files from raw text/dat files
csv_files = suna.files[grepl("\\.csv$", suna.files, ignore.case = TRUE)]
raw_files = suna.files[!grepl("\\.csv$", suna.files, ignore.case = TRUE)]

suna_list = list()

# 2. Process raw text/dat SUNA files with original loader (skip = 15, size check)
if (length(raw_files) > 0) {
  valid_raw = raw_files[file.info(raw_files)$size >= 250000]
  if (length(valid_raw) > 0) {
    suna_list[["raw"]] = load.suna.files(valid_raw, skip = 15)
  }
}

# 3. Process CSV files dynamically (auto-detects delimiter, no skip required)
if (length(csv_files) > 0) {
  csv_data = lapply(csv_files, function(f) {
    df = as.data.frame(data.table::fread(f))
    
    # Ensure Time column is converted to POSIXct UTC if imported as character/numeric
    if ("Time" %in% names(df) && !inherits(df$Time, "POSIXt")) {
      df$Time = as.POSIXct(df$Time, tz = "UTC")
    }
    return(df)
  })
  suna_list[["csv"]] = dplyr::bind_rows(csv_data)
}

# 4. Combine datasets
suna = dplyr::bind_rows(suna_list)

# 5. Apply time threshold and 1-day offset
if (nrow(suna) > 0) {
  suna = suna[suna$Time > make_time_utc(2022), ]
  suna$Time = suna$Time - 86400
  
  saveRDS(suna, file = paste0(directories$save, 'suna.rds'))
  cat("Successfully saved SUNA data with", nrow(suna), "records.\n")
} else {
  warning("No SUNA data met the filtering criteria.")
}