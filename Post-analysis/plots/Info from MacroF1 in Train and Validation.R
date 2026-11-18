### Plot 
# Load required libraries for data manipulation
library(dplyr)
library(tidyr)

# 1. Load the Confusion Matrices
# check.names=FALSE prevents R from altering column names that contain spaces or special characters
# row.names=1 assumes the first column contains your class labels

#### User input: 

modelName = 'DNet169cce4'
dataPath = 'F:/DPI'
outputDir = 'eval_files'
# Create export path cleanly
exportPath   <- file.path(dataPath, outputDir)
if (!dir.exists(exportPath)) {
  dir.create(exportPath, recursive = TRUE)
}

# path setting
modelPath = file.path(dataPath, 'model')
trainmatrix<-file.path(modelPath, paste0(modelName, "_train_confusion.csv"))
train_cm <- read.csv(trainmatrix, row.names = 1, check.names = FALSE)
valmatrix<-file.path(modelPath, paste0(modelName, " confusion.csv"))
val_cm <- read.csv(valmatrix, row.names = 1, check.names = FALSE)

# Convert to numeric matrices
train_mat <- as.matrix(train_cm)
val_mat <- as.matrix(val_cm)

# Helper function to calculate Precision, Recall, and F1 per class
calculate_metrics <- function(cm_matrix) {
  tp <- diag(cm_matrix)
  fp <- colSums(cm_matrix) - tp
  fn <- rowSums(cm_matrix) - tp
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Handle division by zero for classes with 0 predictions or 0 actuals
  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0
  
  data.frame(
    Class = rownames(cm_matrix),
    Support = rowSums(cm_matrix),
    Precision = precision,
    Recall = recall,
    F1 = f1,
    stringsAsFactors = FALSE
  )
}

# --- EVALUATION 1 & 2: Generalization Gaps and Tau Verification ---
train_metrics <- calculate_metrics(train_mat)
val_metrics <- calculate_metrics(val_mat)

gaps <- train_metrics %>%
  inner_join(val_metrics, by = "Class", suffix = c("_Train", "_Val")) %>%
  mutate(F1_Gap = F1_Train - F1_Val) %>%
  arrange(desc(F1_Gap))

cat("\n--- Top 5 Classes with Largest Generalization Gaps (Overfitting) ---\n")
print(head(gaps %>% select(Class, Support_Train, F1_Train, F1_Val, F1_Gap), 10))

# Output Evaluation 1 path
eval1Path <- file.path(exportPath, paste0(modelName, '_F1gap.csv'))
write.csv(gaps,eval1Path)

# --- EVALUATION 3: Feature Ambiguity vs. Pure Memorization ---
# Convert matrices to long format to isolate mistakes (off-diagonals)
extract_mistakes <- function(cm_matrix) {
  as.data.frame(as.table(cm_matrix)) %>%
    rename(True_Class = 1, Predicted_Class = 2, Freq = 3) %>%
    filter(True_Class != Predicted_Class, Freq > 0)
}

train_mistakes <- extract_mistakes(train_mat)
val_mistakes <- extract_mistakes(val_mat)

# Merge to compare training mistakes vs validation mistakes directly
pairwise_comparison <- train_mistakes %>%
  full_join(val_mistakes, by = c("True_Class", "Predicted_Class"), suffix = c("_Train", "_Val")) %>%
  mutate(
    Freq_Train = replace_na(Freq_Train, 0),
    Freq_Val = replace_na(Freq_Val, 0)
  ) %>%
  arrange(desc(Freq_Val))

cat("\n--- Top 10 Validation Confusions (Ambiguity vs Memorization) ---\n")
print(head(pairwise_comparison, 10))

# Output Evaluation 3 path
eval3Path <- file.path(exportPath, paste0(modelName, '_PairComparison.csv'))
write.csv(pairwise_comparison,eval3Path)

# --- EVALUATION 4: Directional Bias and 'Dumping Grounds' ---
# Identify classes predicted significantly more often than their actual frequency
dumping_grounds <- data.frame(
  Class = colnames(val_mat),
  True_Count = rowSums(val_mat),
  Predicted_Count = colSums(val_mat)
) %>%
  filter(True_Count > 0) %>% # Filter out empty classes in validation
  mutate(Overprediction_Ratio = Predicted_Count / True_Count) %>%
  arrange(desc(Overprediction_Ratio))

cat("\n--- Top 5 'Dumping Ground' Classes in Validation ---\n")
print(head(dumping_grounds, 5))

# Output Evaluation 4 path
eval4Path <- file.path(exportPath, paste0(modelName, '_DumpingGrounds.csv'))
write.csv(dumping_grounds,eval4Path)

# --- EVALUATION 5: Flagging Mislabeled Ground Truth Data ---
# Look for the highest absolute frequency of off-diagonal errors in the *training* data
mislabeled_candidates <- train_mistakes %>%
  arrange(desc(Freq)) %>%
  # Calculate what percentage of the true class was misclassified as this specific wrong class
  mutate(Proportion_of_True_Class = Freq / rowSums(train_mat)[True_Class])

cat("\n--- Top 10 Persistent Training Mistakes (Potential Mislabels) ---\n")
print(head(mislabeled_candidates, 10))

# Output Evaluation 53 path
eval5Path <- file.path(exportPath, paste0(modelName, '_HumanError.csv'))
write.csv(mislabeled_candidates,eval5Path)