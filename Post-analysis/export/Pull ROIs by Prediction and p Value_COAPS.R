# last modified: 2026/07/23
setwd('/unity/g1/tchen/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

library(data.table)

#### User input: (muted since they aer defined in ...at once.R)
#outputDir = 'F:/DPI/export/RR2402Cam1c4d1_DenseNet121S1/'
nMax = 1000 
pMin = 0.0 

#camera = 'RR2402-camera1'
#transect = 'cycle4_day1'
#segmentationName = 'REG'
#modelName = 'DenseNet121-S1'

#### Autopilot from here:

sourceFiles = list(
  rawPath = paste0('../../raw/camera0/', transect, '/'),
  segmentationPath = paste0('../../analysis/', camera, '/segmentation/', transect, '-', segmentationName, '/'),
  classificationPath = paste0('../../analysis/', camera, '/classification/', transect, '-', segmentationName, '-', modelName, '/'),
  modelPath = '../model'
)

sourceFiles$segmentationFiles = list.files(sourceFiles$segmentationPath, pattern = 'statistics.csv', recursive = TRUE)
sourceFiles$classificationFiles = list.files(sourceFiles$classificationPath, pattern = 'prediction.csv', recursive = TRUE)

message('Found ', length(sourceFiles$classificationFiles), ' classification results.')

# --- 1. FAST PRIOR CALCULATION ---
nSample = min(100, length(sourceFiles$classificationFiles))
l = sample(1:length(sourceFiles$classificationFiles), size = nSample)
list_of_dfs = list()

message("Starting data load for prior calculation...")
for (i in 1:nSample) {
  if (i %% 10 == 0 || i == nSample) { message(round(i / nSample * 100), '%') }
  file_path = paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[l[i]])
  list_of_dfs[[i]] = fread(file_path, header = TRUE, showProgress = FALSE)
}

classification = as.data.frame(rbindlist(list_of_dfs))
message("Successfully loaded ", nrow(classification), " rows.")

prior = getPrior(classification[,-1])
weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
prior = getPrior(classification[,-1] * weight)

if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = T)
}

results = list()
initTime = Sys.time()

for (i in 1:length(sourceFiles$classificationFiles)) {
  startTime = Sys.time()
  
  predictions = as.data.frame(fread(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[i]), header = T)) 
  
  if (is.null(predictions) || nrow(predictions) == 0 || ncol(predictions) == 0) {
    message("Skipping empty or corrupted file: ", sourceFiles$classificationFiles[i])
    next 
  }
  
  # --- 2. VECTORIZED MATH INSTEAD OF APPLY LOOPS ---
  classes = colnames(predictions)[-1]
  # 'sweep' applies the weights correctly across the columns (MARGIN = 2)
  pred_matrix = sweep(as.matrix(predictions[,-1]), MARGIN = 2, STATS = as.numeric(weight), FUN = "*")
  
  # max.col is much faster than apply(..., which.max)
  pIndex = max.col(pred_matrix, ties.method = "first") 
  
  # Calculate max values directly by row
  pmax = pred_matrix[cbind(1:nrow(pred_matrix), pIndex)] / rowSums(pred_matrix)
  pmax = round(pmax, digits = 2)
  
  # Filter which ones meet our minimum probability
  valid_rows = pmax >= pMin
  
  if(sum(valid_rows) == 0) {
    next # Skip if nothing meets the threshold
  }
  
  # --- 3. FAST UNZIP & INSTANT MOVE ---
  zipName = paste0(sourceFiles$segmentationPath, gsub(' prediction.csv', '.zip', sourceFiles$classificationFiles[i]))
  extractName = paste0(sourceFiles$segmentationPath, gsub(' prediction.csv', '/', sourceFiles$classificationFiles[i]))
  
  files_to_extract = predictions[[1]][valid_rows]
  
  for (class in unique(classes[pIndex[valid_rows]])) {
    class_dir = paste0(outputDir, '/', class)
    if (!dir.exists(class_dir)) dir.create(class_dir, recursive = T)
  }
  
  # FAST UNZIP: Extract everything at once (avoids the O(N*M) search penalty)
  dir.create(extractName, recursive = TRUE, showWarnings = FALSE)
  system(sprintf('tar -xf "%s" -C "%s"', zipName, extractName))
  
  # VECTORIZED FORMATTING: Bypass the slow roundNumber() loop
  formatted_pmax = sprintf("%.2f", pmax[valid_rows])
  
  # Build destination filenames
  dest_files = paste0(outputDir, '/', classes[pIndex[valid_rows]], '/[', formatted_pmax, '] ', gsub('.png', '', files_to_extract), '.png')
  
  # INSTANT MOVE: file.rename is exponentially faster than file.copy()
  file.rename(from = paste0(extractName, '/', files_to_extract), to = dest_files)
  
  # Clean up the unwanted files
  unlink(extractName, recursive = T)
  
  endTime = Sys.time()
  
  results[[i]] = data.frame(filename = dest_files,
                            class = classes[pIndex[valid_rows]],
                            pmax = pmax[valid_rows])
  
  message('Finished file ', i, ' out of ', length(sourceFiles$classificationFiles),
          '.\tElapsed: ', round(as.numeric(endTime) - as.numeric(initTime)), ' sec',
          '\tRemaining: ', round((as.numeric(endTime) - as.numeric(initTime)) / i * (length(sourceFiles$classificationFiles) - i) / 60), ' min')
}

## Subdivide large folders
for (folder in list.dirs(outputDir)[-1]) {
  pulledRois = sample(list.files(folder, pattern = '.png'))
  n = length(pulledRois)
  nFolders = ceiling(n / nMax)
  
  if (nFolders > 1) {
    message('Making subfolder for ', folder, ' (x', nFolders, ')')
    for (nCurrent in 1:nFolders) {
      dir.create(paste0(folder, '/', nCurrent, '/'))
      index = ((nCurrent - 1) * nMax + 1):min(nCurrent * nMax, length(pulledRois))
      file.copy(from = paste0(folder, '/', pulledRois[index]),
                to = paste0(folder, '/', nCurrent, '/', pulledRois[index]))
      file.remove(paste0(folder, '/', pulledRois[index])) 
    }
  }
}

zip(outputDir, zipfile = paste0(outputDir, '.zip'))