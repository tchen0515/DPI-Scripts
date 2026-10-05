### Run the "Pull ROIs..." and Analyze Pull..." at once
# set up the parameters for the following process
#camera = 'RR2402-camera1'
#transect = 'cycle3_day2'
#segmentationName = 'REG'
#modelName = 'DNet121cce2'

### Run the "Pull ROIs..." and Analyze Pull..." at once

# Fetch command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Validate that exactly 4 arguments were passed
if (length(args) != 4) {
  stop("ERROR: You must provide exactly 4 arguments.\n",
       "Usage: Rscript 'pullandAnalyze at once_COAPS.R' <camera> <transect> <segmentName> <modelName>\n",
       "Example: Rscript 'pullandAnalyze at once_COAPS.R' RR2402-camera1 cycle3_day2 REG DNet121cce2", 
       call. = FALSE)
}

# Assign arguments to variables
camera           <- args[1]
transect         <- args[2]
segmentationName <- args[3]
modelName        <- args[4]

# Confirm inputs to the console
message(sprintf("Running pipeline with: Camera=%s | Transect=%s | Seg=%s | Model=%s", 
                camera, transect, segmentationName, modelName))

# The rest of your script continues normally below:
outputDir = paste0('/unity/g1/tchen/DPI/export/',camera,transect,'_',modelName,'/')
# origianl format:  '/unity/g1/tchen/DPI/export/RR2510Cam2c2d2_DenseNet121S1/'

# --- CPU LIMIT CONFIGURATION FOR SHARED NODES in COPAS---
library(parallel)
library(data.table)

# Cap CPU usage to 28 cores maximum (STRICT COAPS POLICY)
total_cores <- parallel::detectCores(logical = TRUE)
cpu_cap <- min(28, floor(total_cores * 0.8))

data.table::setDTthreads(threads = cpu_cap)
Sys.setenv(OMP_NUM_THREADS = cpu_cap)
Sys.setenv(MC_CORES = cpu_cap)

message(sprintf("System has %d cores. Capped R execution to %d threads (80%% cap).", total_cores, cpu_cap))

# Run the first script
setwd('/unity/g1/tchen/DPI/DPI-Scripts/Post-analysis/export/')
source("Pull ROIs by Prediction and p Value_COAPS.R")

# Run the second script after the first one completes
setwd('/unity/g1/tchen/DPI/DPI-Scripts/Post-analysis/processing/')
source("Analyze Pull Results and Match to AVI_COAPS.R")

# Add more scripts as needed
# source("script3.R")
                                  