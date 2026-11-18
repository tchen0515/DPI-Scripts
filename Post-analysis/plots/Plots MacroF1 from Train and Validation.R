### Evaluate model's performance by plots from the confusion matrices (train & validate)
# Load required libraries
library(dplyr)

#### User input: 

modelName = 'CNXcce4'
dataPath = 'F:/DPI'
outputDir = 'export_fig/MacroF1'

# Create export path cleanly
exportPath   <- file.path(dataPath, outputDir)
if (!dir.exists(exportPath)) {
  dir.create(exportPath, recursive = TRUE)
}

# Path setting & Loading data
modelPath = file.path(dataPath, 'model')
trainmatrix<-file.path(modelPath, paste0(modelName, "_train_confusion.csv"))
train_cm <- read.csv(trainmatrix, row.names = 1, check.names = FALSE)
valmatrix<-file.path(modelPath, paste0(modelName, " confusion.csv"))
val_cm <- read.csv(valmatrix, row.names = 1, check.names = FALSE)
logfile <- file.path(modelPath, paste0(modelName, ".log"))
log <-read.table(logfile,sep=',',header = TRUE)

# Convert to numeric matrices
train_mat <- as.matrix(train_cm)
val_mat <- as.matrix(val_cm)

# Helper function to calculate F1 per class
calculate_metrics <- function(cm_matrix) {
  tp <- diag(cm_matrix)
  fp <- colSums(cm_matrix) - tp
  fn <- rowSums(cm_matrix) - tp
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Handle division by zero
  f1[is.na(f1)] <- 0
  
  data.frame(
    Class = rownames(cm_matrix),
    F1 = f1,
    stringsAsFactors = FALSE
  )
}

train_metrics <- calculate_metrics(train_mat)
val_metrics <- calculate_metrics(val_mat)

# Calculate overall Training Macro-F1 (average of all class F1s)
final_train_macro_f1 <- mean(train_metrics$F1)


# ==========================================
# PLOT 1: The "Endpoint Anchor" (Epoch Plot)
# ==========================================

png(file.path(exportPath, paste0(modelName, "_MacroF1_Endpoint_Anchor.png")), width = 800, height = 600, res = 100)
plot(log$epoch,
     log$val_macro_f1,
     ylim = c(0, 1), 
     type = 'l',
     lwd = 2,
     col = 'blue',   
     ylab = 'Macro-F1',
     xlab = 'Epoch',
     main = paste0('Validation Macro-F1 vs Final Training Macro-F1',modelName),
     cex.axis = 2,
     cex.lab = 2)
grid()

# Add the single Endpoint Anchor (Star) at the final epoch
final_epoch <- max(log$epoch)
points(final_epoch, final_train_macro_f1, pch = 8, col = "black", cex = 2.5, lwd = 3)

legend("bottomright", 
       legend = c("Validation Macro-F1", paste0("Final Training Macro-F1 (", round(final_train_macro_f1, 3), ")")),
       col = c("blue", "black"),
       lty = c(1, NA),
       pch = c(NA, 8),
       lwd = c(2, 3),
       bg = "white",
       cex = 2)

dev.off()

# ==========================================
# PLOT 2: Train vs. Validation F1 Scatter Plot
# ==========================================
# Merge training and validation metrics for direct comparison
comparison <- merge(train_metrics, val_metrics, by="Class", suffixes=c("_Train", "_Val"))

png(file.path(exportPath, paste0(modelName, "_F1_Scatter_Plot.png")), width = 800, height = 800, res = 100)

# Create the base scatter plot
plot(comparison$F1_Val, comparison$F1_Train, 
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Validation F1 Score", ylab = "Training F1 Score",
     main = paste0("Generalization Gap: Train vs Val F1 per Class",modelName),
     pch = 19, col = rgb(0.2, 0.4, 0.8, 0.6), cex = 1.5,
     cex.axis = 1.3, cex.lab = 1.3)

# Add the perfect generalization line (y = x)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
grid()

# Optional enhancement: Identify and label severely overfit classes
# (e.g., classes where the model scored over 85% in training, but under 40% in validation)
overfit_classes <- comparison[comparison$F1_Train > 0.85 & comparison$F1_Val < 0.40, ]
if(nrow(overfit_classes) > 0) {
  points(overfit_classes$F1_Val, overfit_classes$F1_Train, pch = 19, col = "red", cex = 1.5)
  text(overfit_classes$F1_Val, overfit_classes$F1_Train, labels = overfit_classes$Class, pos = 4, cex = 0.9, col = "black")
}

legend("bottomright", 
       legend = c("Class", "Severe Overfitting", "Perfect Generalization (y=x)"),
       col = c(rgb(0.2, 0.4, 0.8, 0.6), "red", "red"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 2),
       lwd = c(NA, NA, 2),
       bg = "white",
       cex = 1.2)

dev.off()