library(viridis)
library(fields)
library(mgcv)
library(pals)
library(data.table)
library(lubridate) 
library(dplyr)

## -----------------------------------------------------------------------------
## SETUP & CONFIGURATION
## -----------------------------------------------------------------------------
dataset_name = "cycle1_day2"
save.dir = 'F:/DPI/DPI-sensor/P2402-sensor/trial_run/'
outputDir = 'F:/DPI/export/RR2402Cam1c1d2_kappa121v20/'

final.dir = 'F:/DPI/export_fig/trial_run'
setwd('F:/DPI/export_fig/trial_run')

# 1. Load Sensor & Camera Data
dpi = readRDS(paste0(save.dir,dataset_name, '/_rdata/dpi.rds'))
transects = readRDS(paste0(save.dir,dataset_name, '/_rdata/transects.rds'))
roiIndex = readRDS(paste0(outputDir, 'roiIndex.rds'))


# 2. ADD FILTER HERE: Choose your target category (or categories)

# Option A: Single Category
target_classes <- "appendicularian" 

# Option B: Multiple Categories (uncomment line below if you want multiple)
# target_classes <- c("appendicularian", "copepod", "ctenophore")

## -----------------------------------------------------------------------------
## STEP 1: PARSE TIMESTAMPS & FRAME OFFSETS (PST + Regex Frame Extraction)
## -----------------------------------------------------------------------------
message("1. Locating timestamp strings...")
start_idx <- regexpr("\\d{4}-\\d{2}-\\d{2}\\s\\d{2}-\\d{2}-\\d{2}", roiIndex$filename)

roi_time_str <- ifelse(start_idx > 0, 
                       substr(roiIndex$filename, start_idx, start_idx + 18), 
                       NA_character_)

message("2. Converting strings to POSIXct timestamps...")
avi_start_base <- as.POSIXct(roi_time_str, format = "%Y-%m-%d %H-%M-%S", tz = "UTC")

# Convert California PST (UTC-8) to Sensor UTC by adding 8 hours (8 * 3600 seconds)
avi_start_utc <- avi_start_base + (8 * 3600)

message("3. Extracting frame numbers from filenames...")
# Extracts frame number digits from filename pattern '.avi-XXXXXX-'
extracted_frames <- as.numeric(sub(".*\\.avi-(\\d+)-.*", "\\1", roiIndex$filename))

# Use extracted frame if original frame column is NA; default to 0 if missing
roiIndex$frame_clean <- ifelse(is.na(roiIndex$frame), extracted_frames, as.numeric(roiIndex$frame))
roiIndex$frame_clean[is.na(roiIndex$frame_clean)] <- 0

message("4. Calculating exact ROI timestamps...")
fps <- 19.53 
roiIndex$Time <- avi_start_utc + (roiIndex$frame_clean / fps)

cat("Successfully parsed timestamps for", sum(!is.na(roiIndex$Time)), "out of", nrow(roiIndex), "rows.\n")
message("Updated ROI Time Range (UTC):")
print(range(roiIndex$Time, na.rm = TRUE))

## -----------------------------------------------------------------------------
## STEP 1.5: EXTRACT CATEGORY NUMBER FROM PATH NAME
## -----------------------------------------------------------------------------

# 1. Strip the base directory off the front of every path
# This turns "F:/.../appendicularian/1/1/img.png" into "appendicularian/1/1/img.png"
relative_paths <- sub(outputDir, "", roiIndex$path, fixed = TRUE)

# 2. Extract the folder name (e.g., "appendicularian_fat")
full_class <- sub("/.*", "", relative_paths)

# 3. Strip everything after the first underscore to keep only the major category
roiIndex$class <- sub("_.*", "", full_class)

# 4. Verify the consolidated major categories
cat("Consolidated from sub-classes into", length(unique(roiIndex$class)), "major categories:\n")
print(unique(roiIndex$class))

## -----------------------------------------------------------------------------
## STEP 2: SPATIALLY SYNC ROIs TO SENSOR DISTANCE & DEPTH
## -----------------------------------------------------------------------------
# 1. Combine transects list into one lookup reference
sensor_ref <- bind_rows(transects) %>%
  filter(!is.na(Time) & !is.na(Distance) & !is.na(Depth)) %>%
  arrange(Time)

# 2. Linearly interpolate Distance and Depth for each ROI frame timestamp
roiIndex$Distance <- approx(x = as.numeric(sensor_ref$Time), 
                            y = sensor_ref$Distance, 
                            xout = as.numeric(roiIndex$Time))$y

roiIndex$Depth    <- approx(x = as.numeric(sensor_ref$Time), 
                            y = sensor_ref$Depth, 
                            xout = as.numeric(roiIndex$Time))$y

# 3. Keep only ROIs that fall within active transect spatial boundaries
roi_matched <- roiIndex[!is.na(roiIndex$Distance) & !is.na(roiIndex$Depth), ]

cat("Successfully matched", nrow(roi_matched), "ROIs to spatial coordinates!\n")

## -----------------------------------------------------------------------------
## STEP 3: REBUILD GAM SECTIONS & OVERLAY BIOLOGICAL CATEGORIES
## -----------------------------------------------------------------------------
# Force reset any stale graphics devices
graphics.off()

# 1. GAM Interpolation Function
build_section_gam = function(df, fields, nx = 150, ny = 200, ylim = c(0, 200)) {
  grid_x = seq(min(df$Distance, na.rm = TRUE), max(df$Distance, na.rm = TRUE), length.out = nx)
  grid_y = seq(ylim[1], ylim[2], length.out = ny)
  pred_grid = expand.grid(Distance = grid_x, Depth = grid_y)
  grid_list = list()
  
  for (f in fields) {
    valid = !is.na(df$Distance) & !is.na(df$Depth) & !is.na(df[[f]])
    if (sum(valid) >= 10) {
      mod = mgcv::gam(df[[f]][valid] ~ s(Distance, Depth, k = min(30, sum(valid) - 1)), data = df[valid, ])
      grid_list[[f]] = matrix(predict(mod, newdata = pred_grid), nrow = nx, ncol = ny)
    } else {
      grid_list[[f]] = matrix(NA, nrow = nx, ncol = ny)
    }
  }
  list(x = grid_x, y = grid_y, grid = grid_list)
}

# 2. Filter Subset
roi_matched_sub <- roi_matched[!is.na(roi_matched$class) & 
                                 roi_matched$class %in% target_classes, ]

# 3. Dynamic Color Palette
classes <- sort(unique(roi_matched_sub$class))
num_classes <- length(classes)

if (num_classes <= 26) {
  class_cols <- setNames(pals::alphabet(num_classes), classes)
} else {
  palette_fun <- colorRampPalette(pals::alphabet(26))
  class_cols <- setNames(palette_fun(num_classes), classes)
}

# Track generated plot files for browsing
generated_files <- c()

# 4. Loop Transects and Save Plots directly to final.dir
for (i in seq_along(transects)) {
  df_trans = transects[[i]]
  
  t_min = min(df_trans$Time, na.rm = TRUE)
  t_max = max(df_trans$Time, na.rm = TRUE)
  
  roi_trans = roi_matched_sub[roi_matched_sub$Time >= t_min & 
                                roi_matched_sub$Time <= t_max, ]
  
  fields = names(df_trans)[!names(df_trans) %in% c('Distance', 'Depth', 'Time', 'Longitude', 'Latitude')]
  sec = build_section_gam(df_trans, fields = fields, ylim = c(0, 200))
  
  for (field_name in names(sec$grid)) {
    z_mat = sec$grid[[field_name]]
    if (all(is.na(z_mat))) next
    
    # Save each field as its own individual PNG in final.dir
    png_path <- file.path(final.dir, paste0(dataset_name, '_transect_', i, '_', field_name, '.png'))
    generated_files <- c(generated_files, png_path)
    
    png(png_path, width = 11, height = 8, units = "in", res = 300)
    
    par(mar = c(5, 4, 4, 8))
    
    # Background Heatmap
    fields::image.plot(sec$x, sec$y, z_mat,
                       ylim = c(200, 0),
                       col = viridis::inferno(100),
                       xlab = 'Section Distance (km)',
                       ylab = 'Depth (m)',
                       main = paste0(dataset_name, ' - Transect ', i, ' - ', field_name, 
                                     ' (', paste(target_classes, collapse = ", "), ')'))
    
    contour(sec$x, sec$y, z_mat, add = TRUE, col = adjustcolor("white", alpha.f = 0.3))
    
    # Overlay Biological ROI Abundance (Binned Bubble Plot)
    if (nrow(roi_trans) > 0) {
      
      # 1. Define spatial grid bin sizes (Adjust 'by' values to change resolution)
      dist_step  <- 0.5  # kilometers per bin
      depth_step <- 5    # meters per bin
      
      # 2. Assign ROIs to spatial bins and aggregate counts
      roi_binned <- roi_trans %>%
        mutate(
          dist_bin  = floor(Distance / dist_step) * dist_step + (dist_step / 2),
          depth_bin = floor(Depth / depth_step) * depth_step + (depth_step / 2)
        ) %>%
        group_by(dist_bin, depth_bin, class) %>%
        summarise(count = n(), .groups = "drop")
      
      # 3. Scale point size (cex) proportionally using square-root (area-proportional scaling)
      max_count <- max(roi_binned$count, na.rm = TRUE)
      max_cex   <- 3.0  # Maximum bubble size in inches/units
      min_cex   <- 0.4  # Base bubble size for single detections
      
      roi_binned$cex_scale <- sqrt(roi_binned$count / max_count) * max_cex + min_cex
      
      # 4. Plot abundance bubbles centered at bin midpoints
      points(roi_binned$dist_bin, roi_binned$depth_bin,
             pch = 21,
             bg  = class_cols[as.character(roi_binned$class)],
             col = adjustcolor("black", alpha.f = 0.6),
             cex = roi_binned$cex_scale,
             lwd = 0.4)
      
      # 5. Category Legend
      legend(x = max(sec$x) * 1.02, y = 50,
             legend = names(class_cols),
             fill   = class_cols,
             xpd    = TRUE,
             bty    = "n",
             cex    = 0.7,
             title  = "Category")
      
      # 6. Abundance Size Legend (shows representative counts)
      legend_counts <- round(c(1, max_count * 0.25, max_count * 0.5, max_count))
      legend_cexs   <- sqrt(legend_counts / max_count) * max_cex + min_cex
      
      legend(x = max(sec$x) * 1.02, y = 140,
             legend = legend_counts,
             pt.cex = legend_cexs,
             pch    = 21,
             pt.bg  = "grey70",
             col    = "black",
             xpd    = TRUE,
             bty    = "n",
             cex    = 0.6,
             title  = "Abundance (count)")
    }
    
    dev.off()
  }
}

# Open the first generated plot image automatically
if (length(generated_files) > 0) {
  message("Plots saved to: ", final.dir)
  browseURL(generated_files[1])
}