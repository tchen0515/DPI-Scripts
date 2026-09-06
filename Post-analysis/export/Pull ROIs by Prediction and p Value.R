# last modified: 2025/07/18
setwd('F:/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

#### User input: (muted since they aer defined in ...at once.R)
#outputDir = 'F:/DPI/export/RR2402Cam1c4d1_DenseNet121S1/'
nMax = 1000 # Maximum number of images per pull folder
pMin = 0.0 # minimum probability for pulled images (0 = pull all, 0.9 = 90% confidence)

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

## Calculate priors from full dataset:
#nSample = max(min(500, length(sourceFiles$classificationFiles)), length(sourceFiles$classificationFiles) * 0.2)
#l = sample(1:length(sourceFiles$classificationFiles), size = nSample)
#classification = read.csv(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[l[1]]), header = T)
#for (i in 2:nSample) {
#  message(round(i / nSample * 100), '%')
#  tmp = read.csv(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[l[i]]), header = T)
#  classification = rbind(classification, tmp)
#}
############ Gemini suggestion #######
library(data.table)

# Taking a smaller fixed sample (e.g., 100 files) is usually plenty for a prior and much safer for your RAM.
nSample = min(100, length(sourceFiles$classificationFiles))
l = sample(1:length(sourceFiles$classificationFiles), size = nSample)

# 1. Pre-allocate an empty list to hold the data (Memory Efficient)
list_of_dfs = list()

message("Starting data load for prior calculation...")

# 2. Use your loop for progress reporting
for (i in 1:nSample) {
  # This provides the real-time progress update you requested
  if (i %% 10 == 0 || i == nSample) { 
    message(round(i / nSample * 100), '%')
  }
  
  # Read the file and store it in the list
  file_path = paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[l[i]])
  list_of_dfs[[i]] = fread(file_path, header = TRUE, showProgress = FALSE)
}

# 3. Combine everything at once (Prevents the 64GB error)
classification = as.data.frame(rbindlist(list_of_dfs))

message("Successfully loaded ", nrow(classification), " rows.")

############

prior = getPrior(classification[,-1])

weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
prior = getPrior(classification[,-1] * weight)

#weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
#prior = getPrior(classification[,-1] * weight)

#weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
#prior = getPrior(classification[,-1] * weight)

#weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
#prior = getPrior(classification[,-1] * weight)

#weight = matrix(prior$weight, nrow = 1, ncol = length(prior$weight), byrow = T)
#prior[order(prior$n),]

if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = T)
}

results = list()
initTime = Sys.time()
for (i in 1:length(sourceFiles$classificationFiles)) {
  startTime = Sys.time()
  
#  predictions = read.csv(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[i]), header = T)
  # Wrap it in as.data.frame to ensure the matrix math below it works perfectly
  predictions = as.data.frame(fread(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[i]), header = T)) 
  
  # If the file is 0 bytes, it returns NULL or a 0-row/0-column table. Skip it!
  if (is.null(predictions) || nrow(predictions) == 0 || ncol(predictions) == 0) {
    message("Skipping empty or corrupted file: ", sourceFiles$classificationFiles[i])
    next # The 'next' command tells R to instantly jump to the next 'i' in the loop
  }
  
  ## Extract ROIs
  zipName = paste0(sourceFiles$segmentationPath, gsub(' prediction.csv', '.zip', sourceFiles$classificationFiles[i]))
  extractName = paste0(sourceFiles$segmentationPath, gsub(' prediction.csv', '/', sourceFiles$classificationFiles[i]))
  unzip(zipfile = zipName, exdir = extractName)
  
  classes = colnames(predictions)[-1]
  pmax = apply(predictions[,-1] * weight, 1, function(x){max(x) / sum(x)})
  pmax = round(pmax, digits = 2)
  pIndex = apply(predictions[,-1] * weight, 1, which.max) # Weighted based on prior
  
  for (class in classes) {
    if (!dir.exists(paste0(outputDir, '/', class))) {
      dir.create(paste0(outputDir, '/', class), recursive = T)
    }
  }
  
  l = pmax >= pMin
  
  file.copy(from = paste0(extractName, '/', predictions[[1]])[l], 
            to = paste0(outputDir, '/', classes[pIndex], '/[', roundNumber(pmax), '] ', gsub('.png', '', predictions[[1]]), '.png')[l])
  unlink(extractName, recursive = T)
  endTime = Sys.time()
  
  results[[i]] = data.frame(filename = paste0('[', roundNumber(pmax), '] ', gsub('.png', '', predictions[[1]]), '.png')[l],
                            class = classes[pIndex],
                            pmax = pmax)
  
  message('Finished file ', i, ' out of ', length(sourceFiles$classificationFiles),
          '.\tElapsed time: ', round(as.numeric(endTime) - as.numeric(initTime)), ' sec',
          '\tRemaining time: ', round((as.numeric(endTime) - as.numeric(initTime)) / i * (length(sourceFiles$classificationFiles) - i) / 60), ' min')
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
      file.remove(paste0(folder, '/', pulledRois[index])) # Clean up
    }
  }
}

  zip(outputDir, zipfile = paste0(outputDir, '.zip'))



## Example: fine a particular ROI png from the set of classification files.
#for (i in 1:length(sourceFiles$classificationFiles)) {
#  tmp = loadPrediction(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[i]))
#  if (any('000339-002798.png' == tmp$file)) {
#    print(i)
#    break
#  }
#}

#l = which(tmp$file == '000339-002798.png')
#View(tmp[l,])
