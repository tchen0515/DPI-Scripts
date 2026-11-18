setwd('F:/DPI/DPI-Scripts/Post-analysis/')
source('processing/low level utilities.R')
source('processing/mid level utilities.R')

#### User input: 

modelName = 'CNXcce4'
dataPath = 'F:/DPI/'
outputDir = 'export_fig'
tau_value = '0.33' # Added tau variable for easy adjustment


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

# Output image path (updated name to reflect all metrics)
pngPath <- file.path(exportPath, paste0(modelName, ' Metrics.png'))

# Increased height from 800 to 1200 to accommodate the third plot
png(pngPath, width = 1200, height = 1200, res = 100)

# Changed to mfrow = c(3,1) for Accuracy, Loss, and Macro-F1. 
# Replaced 'plt' with standard 'mar' margins so the 3 plots scale properly.
par(mfrow = c(3,1), mar = c(4, 5, 2, 2), oma = c(0, 0, 3, 0))

# 1. ACCURACY PLOT
plot(log$epoch,
     log$accuracy,
     ylim = c(0, 1), 
     type = 'l',
     lwd = 2,
     col = 'black',
     xaxs = 'i',
     yaxs = 'i',
     ylab = 'Accuracy',
     xlab = 'Epoch',
     xlim = range(pretty(log$epoch)),
     cex.axis = 2,
     cex.lab=2)
lines(log$epoch, log$val_accuracy, lwd = 2, col = 'blue')
lines(runmed(log$epoch, 15), runmed(log$val_accuracy, 15), lwd = 2, col = 'red', lty = 2)
grid()

# Appended tau clarification to the training legend label
legend("bottomright", 
       legend = c(paste0("Training Accuracy (tau=", tau_value, ")"), "Validation Accuracy", "Val Accuracy (15-ep MA)"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bg = "white",
       cex = 2)

# 2. LOSS PLOT
max_loss <- max(c(log$loss, log$val_loss), na.rm = TRUE)

plot(log$epoch,
     log$loss,
     ylim = c(0, min(max_loss, 3)), 
     type = 'l',
     lwd = 2,
     col = 'black',
     xaxs = 'i',
     yaxs = 'i',
     ylab = 'Loss',
     xlab = 'Epoch',
     xlim = range(pretty(log$epoch)),
     cex.axis = 2,
     cex.lab=2)
lines(log$epoch, log$val_loss, lwd = 2, col = 'blue')
lines(runmed(log$epoch, 15), runmed(log$val_loss, 15), lwd = 2, col = 'red', lty = 2)
grid()

# Appended tau clarification to the training legend label
legend("topright", 
       legend = c(paste0("Training Loss (tau=", tau_value, ")"), "Validation Loss", "Val Loss (15-ep MA)"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2,
       bg = "white",
       cex = 2)


# 3. MACRO-F1 PLOT
# Note: The log file only contains 'val_macro_f1', so only the validation curve is plotted here.
plot(log$epoch,
     log$val_macro_f1,
     ylim = c(0, 1), # Scaled 0 to 1 for consistency with accuracy
     type = 'l',
     lwd = 2,
     col = 'blue',   # Using blue to keep validation colors consistent across plots
     xaxs = 'i',
     yaxs = 'i',
     ylab = 'Macro-F1',
     xlab = 'Epoch',
     xlim = range(pretty(log$epoch)),
     cex.axis = 2,
     cex.lab=2)
lines(runmed(log$epoch, 15), runmed(log$val_macro_f1, 15), lwd = 2, col = 'red', lty = 2)
grid()

legend("bottomright", 
       legend = c("Validation Macro-F1", "Val Macro-F1 (15-ep MA)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2,
       bg = "white",
       cex = 2)


# Print the overall title in the outer margin
mtext(modelName, side = 3, outer = TRUE, cex = 2, font = 2)

dev.off()

# Sidecar json reading
sidecar <- jsonlite::read_json(modelJsonFile)
trainingSet <- list.files(sidecar$config$training$scnn_dir, pattern = '.png', full.names = TRUE, recursive = TRUE)
trainingSet <- gsub(sidecar$config$training$scnn_dir, '', trainingSet)