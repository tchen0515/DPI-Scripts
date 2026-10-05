### Run the "Pull ROIs..." and Analyze Pull..." at once
# set up the parameters for the following process

camera = 'RR2402-camera2'
transect = 'cycle4_night5'
segmentationName = 'REG'
modelName = 'DNet121cce2'

outputDir = paste0('F:/DPI/export/',camera,transect,'_',modelName,'/')

# Run the first script
setwd('F:/GithubDesktop/DPI-Scripts/Post-analysis/export/')
source("Pull ROIs by Prediction and p Value_Gemini.R")

# Run the second script after the first one completes
setwd('F:/GithubDesktop/DPI-Scripts/Post-analysis/processing/')
source("Analyze Pull Results and Match to AVI_COAPS.R")

# Add more scripts as needed
# source("script3.R")
                                  