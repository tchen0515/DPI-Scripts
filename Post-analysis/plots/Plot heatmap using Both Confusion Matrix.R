## (Both Trian & val) Represent the confusion matrix in heat map (summarize the confusion matrix first in another code)
library(ggplot2)
library(reshape2)
# library(pheatmap) # Loaded in original, but unused since you are using ggplot2

# 1. Define base paths and model name
base_dir <- 'F:/DPI'
modeln <- 'CNXcce4'

# Ensure export directory exists
if (!dir.exists(file.path(base_dir, 'export_fig'))) {
  dir.create(file.path(base_dir, 'export_fig'), recursive = TRUE)
}

# 2. Define the plotting function
plot_confusion_heatmap <- function(input_csv, output_png, plot_title) {
  # Load data
  cm_df <- read.csv(file.path(base_dir, 'model', input_csv), 
                    row.names = 1, check.names = FALSE)
  
  # Convert to long format for ggplot
  cm_mat <- as.matrix(cm_df)
  cm_long <- melt(cm_mat)
  colnames(cm_long) <- c("Actual", "Predicted", "Count")
  
  # Preserve axis order and reverse Y-axis so diagonal goes top-left to bottom-right
  cm_long$Actual <- factor(cm_long$Actual, levels = rev(rownames(cm_mat)))
  cm_long$Predicted <- factor(cm_long$Predicted, levels = colnames(cm_mat))
  
  # Plot Heatmap
  final_fig <- ggplot(cm_long, aes(x = Predicted, y = Actual, fill = Count)) +
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
      title = plot_title,
      x = "Predicted Class",
      y = "Actual Class",
      fill = "Count (log scale)"
    )
  
  # Export the figure
  out_path <- file.path(base_dir, 'export_fig', output_png)
  ggsave(out_path, plot = final_fig, width = 10, height = 8, dpi = 300)
  cat("Exported heatmap to:", out_path, "\n")
}

# 3. Generate the Validation Heatmap
plot_confusion_heatmap(
  input_csv = paste0(modeln, ' sumconfusion.csv'),
  output_png = paste0(modeln, "_val_heatmap.png"),
  plot_title = paste0("Validation Confusion Matrix Heatmap - ", modeln)
)

# 4. Generate the Training Heatmap
plot_confusion_heatmap(
  input_csv = paste0(modeln, '_train_sumconfusion.csv'),
  output_png = paste0(modeln, "_train_heatmap.png"),
  plot_title = paste0("Training Confusion Matrix Heatmap - ", modeln)
)