setwd('F:/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

#### User input: 

modelName = 'RNet50cce3'
dataPath = 'F:/DPI/'
outputDir = 'export_fig'


#### Autopilot from here:

modelPath = file.path(dataPath, 'model')
modelLogFile <- file.path(modelPath, paste0(modelName, ".log"))
modelJsonFile <- file.path(modelPath, paste0(modelName, ".json"))


# Create export path cleanly
exportPath   <- file.path(dataPath, outputDir)
if (!dir.exists(exportPath)) {
  dir.create(exportPath, recursive = TRUE)
}

log <- read.csv(modelLogFile, header = TRUE)

# Output image path
pngPath <- file.path(exportPath, paste0(modelName, ' Accuracy and Loss.png'))

png(pngPath, width = 1200, height = 800, res = 100)

# Add 'oma' (Outer Margin Area) to create space at the very top of the image
par(mfrow = c(2,1), plt = c(0.15, 0.95, 0.25, 0.95), oma = c(0, 0, 3, 0))

# 1. ACCURACY PLOT
plot(log$epoch,
     log$accuracy,
     ylim = c(0, 1), # Adjusted to fit all accuracy data (0 to 100%)
     type = 'l',
     lwd = 2,
     col = 'black',
     xaxs = 'i',
     yaxs = 'i',
     ylab = 'Accuracy',
     xlab = 'Epoch',
     xlim = range(pretty(log$epoch)))
lines(log$epoch, log$val_accuracy, lwd = 2, col = 'blue')
lines(runmed(log$epoch, 15), runmed(log$val_accuracy, 15), lwd = 2, col = 'red', lty = 2)
grid()

legend("bottomright", 
       legend = c("Training Accuracy", "Validation Accuracy", "Val Accuracy (15-ep MA)"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bg = "white")

# 2. LOSS PLOT
max_loss <- max(c(log$loss, log$val_loss), na.rm = TRUE)

plot(log$epoch,
     log$loss,
     ylim = c(0, min(max_loss, 3)), # Dynamically set to include initial high loss values
     type = 'l',
     lwd = 2,
     col = 'black',
     xaxs = 'i',
     yaxs = 'i',
     ylab = 'Loss',
     xlab = 'Epoch',
     xlim = range(pretty(log$epoch)))
lines(log$epoch, log$val_loss, lwd = 2, col = 'blue')
lines(runmed(log$epoch, 15), runmed(log$val_loss, 15), lwd = 2, col = 'red', lty = 2)
grid()

legend("topright", 
       legend = c("Training Loss", "Validation Loss", "Val Loss (15-ep MA)"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bg = "white")

# Print the overall title in that new outer margin
mtext(modelName, side = 3, outer = TRUE, cex = 1.5, font = 2)

dev.off()

# Sidecar json reading
sidecar <- jsonlite::read_json(modelJsonFile)
trainingSet <- list.files(sidecar$config$training$scnn_dir, pattern = '.png', full.names = TRUE, recursive = TRUE)
trainingSet <- gsub(sidecar$config$training$scnn_dir, '', trainingSet)