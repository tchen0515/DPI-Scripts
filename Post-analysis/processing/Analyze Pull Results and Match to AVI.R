# last modified: 2025/07/18
setwd('F:/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

#### User input: (muted since they aer defined in ...at once.R)
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
# --- ADD THIS SAFETY CHECK ---
if (length(sourceFiles$segmentationFiles) == 0) {
  stop("ERROR: No statistics.csv files found in ", sourceFiles$segmentationPath)
}
# -----------------------------
sourceFiles$classificationFiles = list.files(sourceFiles$classificationPath, pattern = 'prediction.csv')


## Load in list of all files in directory:
roiFiles = list.files(outputDir, pattern = '.png', full.names = T, recursive = T)
message('Found ', length(roiFiles), ' image files.')

roiIndex = data.frame(path = roiFiles,
                      class = NA,
                      filename = NA,
                      frame = NA,
                      roi = NA,
                      camera = camera,
                      transect = transect,
                      segmentationName = NA,
                      modelName = modelName,
                      aviFile = NA,
                      time = NA,
                      width = NA,
                      height = NA)

roiFiles = gsub(outputDir, '', roiFiles)
roiFiles = strsplit(roiFiles, split = '/')


for (i in 1:nrow(roiIndex)) {
  if (length(roiFiles[[i]]) > 1) {
    n = length(roiFiles[[i]])
    roiIndex$class[i] = roiFiles[[i]][n-1]
    
    tmp = strsplit(roiFiles[[i]][n], split = ' ')[[1]]
    roiIndex$filename[i] = gsub('.png', '', tmp[2])
    
    ## get frame and roi number
    tmp = strsplit(roiIndex$filename[i], split = '-')[[1]]
    roiIndex$frame[i] = as.numeric(tmp[1])
    roiIndex$roi[i] = as.numeric(tmp[2])
    
    #tmp = png::readPNG(source = roiIndex$path[i])
    #roiIndex$height[i] = dim(tmp)[1]
    #roiIndex$width[i] = dim(tmp)[2]
    # Safely try to read the image
    tmp = tryCatch({
      png::readPNG(source = roiIndex$path[i])
    }, error = function(e) {
      NULL # If it fails, return NULL instead of crashing
    })
    
    # Only assign dimensions if the image loaded successfully
    if (!is.null(tmp)) {
      roiIndex$height[i] = dim(tmp)[1]
      roiIndex$width[i] = dim(tmp)[2]
    } else {
      message("Warning: Skipping corrupted image at index ", i, ": ", roiIndex$path[i])
    }
    
  }
}

View(roiIndex)

## Load segmentation statistics files:
statistics = loadMeasurement(paste0(sourceFiles$segmentationPath, '/', sourceFiles$segmentationFiles[1]))
statistics$size = apply(statistics[,c('w','h')], 1, max)
statistics = statistics[statistics$size >= min(roiIndex$width, na.rm = T), ] # only want to actually look at the larger ROIs (perimeter > 100).

#if (length(sourceFiles$segmentationFiles) > 1) {
#  for (i in 2:length(sourceFiles$segmentationFiles)) {
#    message('Loading statistics file ', i)
#    tmp = loadMeasurement(paste0(sourceFiles$segmentationPath, '/', sourceFiles$segmentationFiles[i]))
#    tmp$size = apply(tmp[,c('w','h')], 1, max)
#    tmp = tmp[tmp$size >= min(roiIndex$width, na.rm = T), ] # only want to actually look at the larger ROIs (perimeter > 100).
    
#    statistics = rbind(statistics, tmp)
#  }
#}
############ Gemini suggestion #######
library(data.table)

# Use list.files with full.names = T to avoid the "//" permission error
stat_files = list.files(sourceFiles$segmentationPath, pattern = 'statistics.csv', full.names = TRUE, recursive = TRUE)

message('Loading ', length(stat_files), ' statistics files...')

# Read and process all files in a list
stat_list = lapply(stat_files, function(f) {
  tmp = loadMeasurement(f)
  # Basic error handling: if file is empty or fails, return NULL
  if(is.null(tmp) || nrow(tmp) == 0) return(NULL)
  
  tmp$size = apply(tmp[,c('w','h')], 1, max)
  # Filter early to save memory
  return(tmp[tmp$size >= min(roiIndex$width, na.rm = T), ])
})

# Bind them all at once (Filter out NULLs first)
statistics = as.data.frame(rbindlist(stat_list[!sapply(stat_list, is.null)]))
######################################

#for (i in 1:nrow(roiIndex)) {
#  if (i %% 1000 == 0) {
#    message('.')
#  }
#  k = which(roiIndex$frame[i] == statistics$frame & roiIndex$roi[i] == statistics$crop & roiIndex$width[i] == statistics$size)
  
#  if (length(k) > 1) {
#    message('Name collision found (n=', length(k), ').')
#  } else if (length(k) == 1) {
#    roiIndex$aviFile[i] = statistics$filename[k]
#    #statistics = statistics[-k,]
#  } else {
#    message('No matching particle information found for ', roiIndex$filename[i])
#  }
#}
############ Gemini suggestion #######
# Rename 'crop' in statistics to 'roi' so the columns match for the join
names(statistics)[names(statistics) == "crop"] <- "roi"

# Perform a left join to instantly match all rows based on frame, roi, and size/width
roiIndex = merge(
  x = roiIndex, 
  y = statistics[, c("frame", "roi", "size", "filename")], 
  by.x = c("frame", "roi", "width"), 
  by.y = c("frame", "roi", "size"), 
  all.x = TRUE
)

# The merge adds the matching 'filename' from statistics. We just need to move it to 'aviFile'
roiIndex$aviFile = roiIndex$filename.y
roiIndex$filename.y = NULL # Clean up the extra column
names(roiIndex)[names(roiIndex) == "filename.x"] <- "filename" # Restore original name
############ 

saveRDS(roiIndex, file = paste0(outputDir, '/roiIndex.rds'))
