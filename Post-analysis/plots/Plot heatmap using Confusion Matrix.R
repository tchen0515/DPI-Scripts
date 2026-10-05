## Represent the confusion matrix in heat map (summarize the confusion matrix first in another code)
library(ggplot2)
library(reshape2)
library(pheatmap)
setwd('D:/OneDrive - Florida State University/DPI/model/')

# 1. Load data
modeln='DNet121cce2'
cm_df <- read.csv(paste0(modeln,' sumconfusion.csv')
                  , row.names = 1, check.names = FALSE)

# 2. Convert to long format for ggplot
cm_mat <- as.matrix(cm_df)
cm_long <- melt(cm_mat)
colnames(cm_long) <- c("Actual", "Predicted", "Count")

# Preserve axis order and reverse Y-axis so diagonal goes top-left to bottom-right
cm_long$Actual <- factor(cm_long$Actual, levels = rev(rownames(cm_mat)))
cm_long$Predicted <- factor(cm_long$Predicted, levels = colnames(cm_mat))

# 3. Plot Heatmap
ggplot(cm_long, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma", trans = "log1p", na.value = "white") +
  coord_fixed() +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = paste0("Confusion Matrix Heatmap_",modeln),
    x = "Predicted Class",
    y = "Actual Class",
    fill = "Count (log scale)"
  )