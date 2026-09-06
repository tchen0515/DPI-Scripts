### Run the "Pull ROIs..." and Analyze Pull..." at once
# set up the parameters for the following process

outputDir = 'F:/DPI/export/RR2510Cam2c2d2_DenseNet121S1/' 

camera = 'RR2510-camera2'
transect = 'cycle2_day2'
segmentationName = 'REG'
modelName = 'DenseNet121-S1'


# Run the first script
setwd('F:/DPI/DPI-Scripts/Post-analysis/export/')
source("Pull ROIs by Prediction and p Value_Gemini.R")

# Run the second script after the first one completes
setwd('F:/DPI/DPI-Scripts/Post-analysis/processing/')
source("Analyze Pull Results and Match to AVI_Gemini.R")

# Add more scripts as needed
# source("script3.R")
                                  