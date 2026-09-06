# last modified: 2026/07/23
setwd('F:/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

library(data.table)
library(magick) # Required for fast image metadata reading

#### User input: (muted since they are defined in ...at once.R)
#outputDir = 'F:/DPI/export/RR2402Cam1c4d1_DenseNet121S1/'
#camera = 'RR2402-camera1'
#transect = 'cycle4_day1'
#segmentationName = 'REG'
#modelName = 'DenseNet121-S1'

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
roiFiles = list.files(outputDir, pattern = '.png', full.names = T, recursive = T)
message('Found ', length(roiFiles), ' image files.')

# Create the base dataframe
roiIndex = data.frame(path = roiFiles,
                      camera = camera,
                      transect = transect,
                      segmentationName = NA,
                      modelName = modelName,
                      aviFile = NA,
                      time = NA)

# --- 1. VECTORIZED STRING MANIPULATION ---
# Clean paths
relative_paths = gsub(outputDir, '', roiIndex$path)
path_splits = strsplit(relative_paths, split = '/')

# Extract class (second to last element)
roiIndex$class = sapply(path_splits, function(x) { if(length(x) > 1) x[length(x)-1] else NA })

# Extract filename
filenames_raw = sapply(path_splits, function(x) { if(length(x) > 1) x[length(x)] else NA })
filenames_split = strsplit(filenames_raw, split = ' ')
roiIndex$filename = sapply(filenames_split, function(x) { if(length(x) > 1) gsub('.png', '', x[2]) else NA })

# Extract frame and roi
frame_roi = strsplit(roiIndex$filename, split = '-')
roiIndex$frame = as.numeric(sapply(frame_roi, function(x) { if(length(x) > 1) x[1] else NA }))
roiIndex$roi = as.numeric(sapply(frame_roi, function(x) { if(length(x) > 1) x[2] else NA }))

# --- 2. FAST IMAGE DIMENSION READING ---
message('Reading image dimensions...')
# image_info only reads the header, skipping the heavy pixel loading
img_metadata = magick::image_info(magick::image_read(roiIndex$path))
roiIndex$width = img_metadata$width
roiIndex$height = img_metadata$height

# --- 3. FAST SEGMENTATION FILE LOADING ---
stat_files = list.files(sourceFiles$segmentationPath, pattern = 'statistics.csv', full.names = TRUE, recursive = TRUE)
message('Loading ', length(stat_files), ' statistics files...')

stat_list = lapply(stat_files, function(f) {
  tmp = loadMeasurement(f)
  if(is.null(tmp) || nrow(tmp) == 0) return(NULL)
  tmp$size = apply(tmp[,c('w','h')], 1, max)
  return(tmp[tmp$size >= min(roiIndex$width, na.rm = T), ])
})

statistics = as.data.frame(rbindlist(stat_list[!sapply(stat_list, is.null)]))

# --- 4. INSTANT LEFT JOIN ---
names(statistics)[names(statistics) == "crop"] <- "roi"
roiIndex = merge(
  x = roiIndex, 
  y = statistics[, c("frame", "roi", "size", "filename")], 
  by.x = c("frame", "roi", "width"), 
  by.y = c("frame", "roi", "size"), 
  all.x = TRUE
)

roiIndex$aviFile = roiIndex$filename.y
roiIndex$filename.y = NULL 
names(roiIndex)[names(roiIndex) == "filename.x"] <- "filename" 

saveRDS(roiIndex, file = paste0(outputDir, '/roiIndex.rds'))
message('Done!')