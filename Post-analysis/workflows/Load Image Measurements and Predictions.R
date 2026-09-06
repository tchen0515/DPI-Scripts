setwd('F:/DPI/DPI-Scripts _noG/Post-analysis')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

#### User input: 

camera = 'RR2510-camera2'
transect = 'cycle2_day2'
segmentationName = 'REG'
modelName = 'DNet121cce2'
minPerimeter = 100

#### Autopilot from here:

sourceFiles = list(
  rawPath = paste0('../../raw/camera0/', transect, '/'),
  segmentationPath = paste0('../../analysis/', camera, '/segmentation/', transect, '-', segmentationName, '/'),
  classificationPath = paste0('../../analysis/', camera, '/classification/', transect, '-', segmentationName, '-', modelName, '/'),
  modelPath = '../model'
)

sourceFiles$segmentationFiles = list.files(sourceFiles$segmentationPath, pattern = 'statistics.csv')
sourceFiles$classificationFiles = list.files(sourceFiles$classificationPath, pattern = 'prediction.csv')
sourceFiles$rawFiles = list.files(sourceFiles$rawPath, pattern = '.avi')


measurement = loadMeasurement(paste0(sourceFiles$segmentationPath, sourceFiles$segmentationFiles[1]))
measurement = measurement[measurement$w + measurement$h >= minPerimeter/2,]

for (i in 2:length(sourceFiles$segmentationFiles)) {
  tmp = loadMeasurement(paste0(sourceFiles$segmentationPath, sourceFiles$segmentationFiles[i]))
  tmp = tmp[tmp$w + tmp$h >= minPerimeter/2,]
  
  measurement = rbind(measurement, tmp)
}


prediction = loadPrediction(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[1]))

for (i in 2:length(sourceFiles$segmentationFiles)) {
  tmp = loadPrediction(paste0(sourceFiles$classificationPath, sourceFiles$classificationFiles[i]))
  prediction = rbind(prediction, tmp)
}

prediction = classify(prediction)

measurement = mergeMeasurementClassification(measurement, prediction)
measurementClassified = measurement[!is.na(measurement$class),]
