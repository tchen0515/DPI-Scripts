# last modified: 2026/09/14

# --- Working Directory ---
setwd('/unity/g1/tchen/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

library(data.table)
# Ensure standalone runs also observe the 30 CPU cap
data.table::setDTthreads(threads = 28)
library(magick) 

# --- DUAL-COMPATIBLE INPUT HANDLING ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 4) {
  # Scenario A: Running standalone via terminal
  camera           <- args[1]
  transect         <- args[2]
  segmentationName <- args[3]
  modelName        <- args[4]
  outputDir        <- paste0('/unity/g1/tchen/DPI/export/', camera, transect, '_', modelName, '/')
  message(sprintf("Standalone mode: Camera=%s | Transect=%s", camera, transect))
  
} else if (exists("camera") && exists("transect") && exists("segmentationName") && exists("outputDir")) {
  # Scenario B: Sourced from master script
  message(sprintf("Master script mode: Inheriting Camera=%s | Transect=%s", camera, transect))
  
} else {
  stop("ERROR: Missing variables. Either run via master script or provide 4 CLI arguments.")
}

#### Autopilot from here:

sourceFiles = list(
  rawPath = paste0('../../raw/', camera, '/', transect, '/'),
  segmentationPath = paste0('../../analysis/', camera, '/segmentation/', transect, '-', segmentationName, '/'),
  classificationPath = paste0('../../analysis/', camera, '/classification/', transect, '-', segmentationName, '-', modelName, '/'),
  modelPath = '../model'
)

sourceFiles$segmentationFiles = list.files(sourceFiles$segmentationPath, pattern = 'statistics.csv', recursive = TRUE)

if (length(sourceFiles$segmentationFiles) == 0) {
  stop("ERROR: No statistics.csv files found in ", sourceFiles$segmentationPath)
}

sourceFiles$classificationFiles = list.files(sourceFiles$classificationPath, pattern = 'prediction.csv')

## Load in list of all files in directory:
roiFiles = list.files(outputDir, pattern = '\\.png$', full.names = TRUE, recursive = TRUE)
message('Found ', length(roiFiles), ' image files.')

# Stop early if no images are found
if (length(roiFiles) == 0) {
  stop(sprintf("ERROR: No .png files found in '%s'.", outputDir))
}

# Create the base dataframe
roiIndex = data.frame(path = roiFiles,
                      camera = camera,
                      transect = transect,
                      segmentationName = NA,
                      modelName = modelName,
                      aviFile = NA_character_,
                      time = NA)

# --- 1. FAST VECTORIZED STRING PARSING ---
setDT(roiIndex)

relative_paths <- gsub(outputDir, '', roiIndex$path)
path_parts <- tstrsplit(relative_paths, "/", fixed = TRUE)

# Safe-guard subfolder indexing
target_idx <- max(1, length(path_parts) - 1)
roiIndex[, class := path_parts[[target_idx]]]

# Extract clean filename
raw_filenames <- basename(roiIndex$path)
clean_names <- sub("^\\[[0-9.]+\\] ", "", raw_filenames) 
clean_names <- sub("\\.png$", "", clean_names)

roiIndex[, filename := clean_names]

# Extract frame and roi from the end of the string using regex
# \\1 captures the specific group of digits
roiIndex[, frame := as.numeric(sub(".*-([0-9]+)-[0-9]+$", "\\1", clean_names))]
roiIndex[, roi := as.numeric(sub(".*-[0-9]+-([0-9]+)$", "\\1", clean_names))]

# --- 2. FAST SEGMENTATION FILE LOADING WITH REAL-TIME PROGRESS ---
# Search recursively for any file ending in 'statistics.csv' (handles subfolders & spaces)
stat_files = list.files(sourceFiles$segmentationPath, 
                        pattern = 'statistics.*\\.csv$', 
                        full.names = TRUE, 
                        recursive = TRUE)

n_files = length(stat_files)
message('Loading ', n_files, ' statistics files...')
stat_list = vector("list", n_files)
start_time = Sys.time()

for (i in seq_along(stat_files)) {
  f = stat_files[i]
  tmp = loadMeasurement(f)
  
  if (!is.null(tmp) && nrow(tmp) > 0) {
    stat_list[[i]] = tmp
  }
  
  # Calculate real-time elapsed and remaining time
  elapsed_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  avg_per_file = elapsed_sec / i
  remaining_sec = avg_per_file * (n_files - i)
  remaining_min = round(remaining_sec / 60)
  
  message(sprintf("Finished file %d out of %d.     Elapsed: %d sec     Remaining: %d min", 
                  i, n_files, round(elapsed_sec), remaining_min))
}

statistics = rbindlist(stat_list[!sapply(stat_list, is.null)], fill = TRUE)

# --- 3. INSTANT DATA.TABLE JOIN ---
setnames(statistics, "crop", "roi", skip_absent = TRUE)

# Validate that the required columns actually exist in the loaded CSVs
required_cols <- c("frame", "roi", "filename")
missing_cols <- setdiff(required_cols, names(statistics))

if (length(missing_cols) > 0) {
  stop(sprintf("\nERROR: Join failed! The statistics.csv files are missing required column(s): %s. Please check the raw CSV files for formatting errors.", 
               paste(missing_cols, collapse = ", ")))
}

# Join on frame and roi without opening PNG files
roiIndex[statistics, aviFile := i.filename, on = .(frame, roi)]
saveRDS(roiIndex, file = paste0(outputDir, '/roiIndex.rds'))
message('Done!')