## Plot the accuracy & loss separately for training and validation metrics
# Path setting & Loading data (User Input)
modelName = 'CNXcce4'
dataPath = 'F:/DPI/'
outputDir = 'export_fig/TrainAndLoss'
tau=0.33

# Create export path cleanly
exportPath <- file.path(dataPath, outputDir)
if (!dir.exists(exportPath)) {
  dir.create(exportPath, recursive = TRUE)
}

# Load log file
modelPath = file.path(dataPath, 'model')
logfile <- file.path(modelPath, paste0(modelName, ".log")) 
log <- read.table(logfile, sep=',', header = TRUE)


# ==========================================
# METHOD 1: Dual Y-Axes Plot
# ==========================================
png(file.path(exportPath, paste0(modelName, "_Loss_DualAxes.png")), width = 800, height = 600, res = 100)

par(mar = c(5, 5, 4, 5) + 0.1) 

# Plot Training Loss (Left Axis)
plot(log$epoch, log$loss, type = "l", col = "blue", lwd = 2,
     xlab = "Epoch", ylab = "Training Loss (Blue)", 
     main = paste0("Training vs Validation Loss (Dual Y-Axes)-",modelName),
     cex.lab = 1.3, cex.axis = 1.2)
grid()

# Plot Validation Loss (Right Axis)
par(new = TRUE)
plot(log$epoch, log$val_loss, type = "l", col = "red", lwd = 2,
     axes = FALSE, xlab = "", ylab = "")
axis(4, cex.axis = 1.2, col.axis="red")
mtext("Validation Loss (Red)", side = 4, line = 3, cex = 1.3, col="red")

# Added legend with tau to Train only and cex=2
legend("topright", 
       legend = c(paste0("Train Loss (tau=", tau, ")"), "Val Loss"),
       col = c("blue", "red"), lty = 1, lwd = 2, bg = "white", cex = 1.2)

dev.off()


# ==========================================
# METHOD 2: Normalized Loss (Min-Max Scaling)
# ==========================================
png(file.path(exportPath, paste0(modelName, "_Loss_Normalized.png")), width = 800, height = 600, res = 100)

normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }

log$loss_norm <- normalize(log$loss)
log$val_loss_norm <- normalize(log$val_loss)

par(mar = c(5, 5, 4, 2) + 0.1)

plot(log$epoch, log$loss_norm, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1),
     xlab = "Epoch", ylab = "Normalized Loss (0 to 1)",
     main = paste0("Training vs Validation Loss (Min-Max Normalized)-",modelName),
     cex.lab = 1.3, cex.axis = 1.2)

lines(log$epoch, log$val_loss_norm, col = "red", lwd = 2)
grid()

# Added legend with tau to Train only and cex=2
legend("topright", 
       legend = c(paste0("Normalized Train Loss (tau=", tau, ")"), "Normalized Val Loss"),
       col = c("blue", "red"), lty = 1, lwd = 2, bg = "white", cex = 1.2)

dev.off()


# ==========================================
# METHOD 3: Separate Subplots (Stacked)
# ==========================================
png(file.path(exportPath, paste0(modelName, "_Loss_Subplots.png")), width = 800, height = 800, res = 100)

par(mfrow = c(2, 1), mar = c(4, 5, 3, 2) + 0.1) 

# Top Panel: Training Loss
plot(log$epoch, log$loss, type = "l", col = "blue", lwd = 2,
     xlab = "", ylab = "Training Loss", 
     main = paste0("Training Loss over Epochs-",modelName),
     cex.lab = 1.3, cex.axis = 1.2)
grid()
# Added legend with tau and cex=2
legend("topright", legend = paste0("Train Loss (tau=", tau, ")"), 
       col = "blue", lty = 1, lwd = 2, bg = "white", cex = 1.2)

# Bottom Panel: Validation Loss
plot(log$epoch, log$val_loss, type = "l", col = "red", lwd = 2,
     xlab = "Epoch", ylab = "Validation Loss", 
     main = paste0("Validation Loss over Epochs-",modelName),
     cex.lab = 1.3, cex.axis = 1.2)
grid()
# Added base legend for validation and cex=2
legend("topright", legend = "Val Loss", 
       col = "red", lty = 1, lwd = 2, bg = "white", cex = 1.2)

par(mfrow = c(1, 1)) 

dev.off()