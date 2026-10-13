library(viridis)
library(ggplot2)
library(maps)
library(mgcv)
library(fields)

## SETUP & CONFIGURATION (Should be done in Sensor A.R)
## -----------------------------------------------------------------------------
#dataset_name = "cycle1_day2"

# Automatically build directory paths from dataset_name if root.dir isn't pre-loaded
if (!exists("root.dir")) {
  root.dir = paste0("F:/DPI/DPI-sensor/P2402-sensor/sensor_data/", dataset_name, "/")
}
save.dir = paste0(root.dir, "_rdata/")

# Load merged transect data
transects = readRDS(paste0(save.dir, 'transects.rds'))
world_map = map_data("world")

## -----------------------------------------------------------------------------
## 1. MAPS
## -----------------------------------------------------------------------------
map_pdf_path = paste0(save.dir, dataset_name, ' Maps.pdf')
pdf(map_pdf_path, width = 8, height = 6)

all_transects_list = lapply(seq_along(transects), function(i) {
  df = transects[[i]]
  df$Transect = as.factor(i)
  return(df)
})
all_transects = do.call(rbind, all_transects_list)

lon_min = min(all_transects$Longitude, na.rm = TRUE) - 0.5
lon_max = max(all_transects$Longitude, na.rm = TRUE) + 0.5
lat_min = min(all_transects$Latitude, na.rm = TRUE) - 0.5
lat_max = max(all_transects$Latitude, na.rm = TRUE) + 0.5

transect_labels = aggregate(cbind(Longitude, Latitude) ~ Transect, data = all_transects, FUN = mean, na.rm = TRUE)

p_all = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "antiquewhite", color = "grey50") +
  geom_path(data = all_transects, aes(x = Longitude, y = Latitude, color = Transect), linewidth = 1.2) +
  geom_text(data = transect_labels, aes(x = Longitude, y = Latitude, label = Transect, color = Transect), 
            fontface = "bold", vjust = -0.7, show.legend = FALSE) +
  coord_quickmap(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  scale_color_manual(values = pals::alphabet(length(transects))) +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", title = paste(dataset_name, "- All Transects"))

print(p_all)

for (i in seq_along(transects)) {
  df_i = all_transects[all_transects$Transect == as.character(i), ]
  label_i = transect_labels[transect_labels$Transect == as.character(i), ]
  col_i = pals::alphabet(length(transects))[i]
  
  p_ind = ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "antiquewhite", color = "grey50") +
    geom_path(data = df_i, aes(x = Longitude, y = Latitude), color = col_i, linewidth = 1.2) +
    geom_text(data = label_i, aes(x = Longitude, y = Latitude, label = Transect), color = col_i, 
              fontface = "bold", vjust = -0.7) +
    coord_quickmap(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
    theme_bw() +
    labs(x = "Longitude", y = "Latitude", title = paste(dataset_name, "- Transect", i))
  
  print(p_ind)
}
dev.off()

browseURL(map_pdf_path)

## -----------------------------------------------------------------------------
## 2. PRELIMINARY SECTION PLOTS (With Color Bar)
## -----------------------------------------------------------------------------
for (i in seq_along(transects)) {
  prelim_pdf_path = paste0(save.dir, dataset_name, ' Section Plots Preliminary (transect ', i, ').pdf')
  pdf(prelim_pdf_path, width = 10, height = 8)
  par(mfrow = c(2, 2))
  
  df_i = transects[[i]]
  
  for (j in seq_len(ncol(df_i))) {
    vals = suppressWarnings(as.numeric(df_i[[j]]))
    var_name = names(df_i)[j]
    
    finite_vals = vals[is.finite(vals)]
    
    if (length(finite_vals) > 0 && !var_name %in% c("Time", "Distance", "Depth")) {
      val_range = range(finite_vals)
      if (val_range[1] == val_range[2]) {
        val_range = val_range + c(-0.01, 0.01)
      }
      
      val_cut = cut(vals, breaks = seq(val_range[1], val_range[2], length.out = 101), include.lowest = TRUE)
      col_vec = viridis::inferno(100)[as.numeric(val_cut)]
      
      par(mar = c(4, 4, 3, 5))
      
      plot(df_i$Latitude, df_i$Depth,
           pch = 16, cex = 0.5,
           col = col_vec,
           ylim = c(100, 0),
           xlab = "Latitude", ylab = "Depth (m)",
           main = paste(var_name, "- Transect", i))
      
      fields::image.plot(legend.only = TRUE, 
                         zlim = val_range, 
                         col = viridis::inferno(100), 
                         smallplot = c(0.91, 0.93, 0.15, 0.85))
    }
  }
  
  dev.off()
}

## -----------------------------------------------------------------------------
## 3. BUILD SECTIONS (GAM Interpolation)
## -----------------------------------------------------------------------------
build_section_gam = function(df, fields, nx = 150, ny = 200, ylim = c(0, 200)) {
  dist_vec = suppressWarnings(as.numeric(df$Distance))
  depth_vec = suppressWarnings(as.numeric(df$Depth))
  
  valid_dist = dist_vec[is.finite(dist_vec)]
  
  if (length(valid_dist) < 2 || max(valid_dist) <= min(valid_dist)) {
    x_min = 0
    x_max = 1
  } else {
    x_min = min(valid_dist)
    x_max = max(valid_dist)
  }
  
  grid_x = seq(x_min, x_max, length.out = nx)
  grid_y = seq(min(ylim), max(ylim), length.out = ny)
  pred_grid = expand.grid(Distance = grid_x, Depth = grid_y)
  
  grid_list = list()
  
  for (f in fields) {
    if (f %in% names(df)) {
      vals = suppressWarnings(as.numeric(df[[f]]))
      valid = is.finite(dist_vec) & is.finite(depth_vec) & is.finite(vals)
      
      if (sum(valid) >= 10) {
        fit_df = data.frame(Distance = dist_vec[valid], Depth = depth_vec[valid], Var = vals[valid])
        mod = tryCatch({
          mgcv::gam(Var ~ s(Distance, Depth, k = min(30, sum(valid) - 1)), data = fit_df)
        }, error = function(e) NULL)
        
        if (!is.null(mod)) {
          preds = predict(mod, newdata = pred_grid)
          # Replace non-finite GAM predictions (Inf, -Inf, NaN) with NA
          preds[!is.finite(preds)] = NA
          grid_mat = matrix(preds, nrow = nx, ncol = ny)
        } else {
          grid_mat = matrix(NA, nrow = nx, ncol = ny)
        }
      } else {
        grid_mat = matrix(NA, nrow = nx, ncol = ny)
      }
    } else {
      grid_mat = matrix(NA, nrow = nx, ncol = ny)
    }
    grid_list[[f]] = grid_mat
  }
  
  list(x = grid_x, y = grid_y, grid = grid_list)
}

section = list()

for (i in seq_along(transects)) {
  message(paste("Processing section for transect", i))
  fields = names(transects[[i]])
  fields = fields[!fields %in% c('Distance', 'Depth', 'Time', 'Longitude', 'Latitude')]
  
  section[[i]] = build_section_gam(transects[[i]], fields = fields, ylim = c(0, 200))
}


## -----------------------------------------------------------------------------
## 4. PLOT SECTIONS (With Image Color Bar & Station Markers)
## -----------------------------------------------------------------------------

# 1. Close any hanging background PDF devices from previous crashes
graphics.off()

for (i in seq_along(section)) {
  sec = section[[i]]
  
  skip_fields = c('Timestamp', 'YearAndDayOfYear', 'HoursOfDay', 'Distance', 
                  'Depth', 'Time', 'Longitude', 'Latitude', 'AccelerationX', 
                  'AccelerationY', 'AccelerationZ', 'Roll', 'Pitch')
  valid_fields = setdiff(names(sec$grid), skip_fields)
  
  # Skip generating a PDF entirely if there are no valid fields
  if (length(valid_fields) == 0) next
  
  section_pdf_path = paste0(save.dir, dataset_name, ' Section Plots (transect ', i, ').pdf')
  pdf(section_pdf_path, width = 10, height = 8)
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 5))
  
  # Track if we actually draw anything to prevent 0-page corrupted PDFs
  plots_drawn = 0 
  
  for (field_name in valid_fields) {
    # 2. tryCatch guarantees that if one plot fails, it won't crash dev.off()
    tryCatch({
      z_mat = sec$grid[[field_name]]
      z_mat[!is.finite(z_mat)] = NA
      z_finite = z_mat[is.finite(z_mat)]
      
      if (length(z_finite) > 0) {
        z_range = suppressWarnings(range(z_finite, na.rm = TRUE))
        
        if (all(is.finite(z_range))) {
          if (z_range[1] == z_range[2]) {
            z_range = z_range + c(-0.01, 0.01)
          }
          
          x_vec = sec$x
          y_vec = sec$y
          
          if (!is.numeric(x_vec) || any(!is.finite(x_vec)) || any(diff(x_vec) <= 0) || length(x_vec) != nrow(z_mat)) {
            x_vec = seq(0, 1, length.out = nrow(z_mat))
          }
          if (!is.numeric(y_vec) || any(!is.finite(y_vec)) || any(diff(y_vec) <= 0) || length(y_vec) != ncol(z_mat)) {
            y_vec = seq(0, max(sec$y, 200, na.rm = TRUE), length.out = ncol(z_mat))
          }
          
          z_plot = z_mat[, ncol(z_mat):1, drop = FALSE]
          
          fields::image.plot(x_vec, y_vec, z_plot,
                             zlim = z_range,
                             ylim = range(y_vec),
                             yaxt = "n",
                             col = viridis::inferno(100),
                             xlab = 'Section Distance (km)',
                             ylab = 'Depth (m)',
                             main = paste0(dataset_name, ' - Transect ', i, ' - ', field_name))
          
          y_ticks = pretty(y_vec)
          axis(2, at = y_ticks, labels = rev(y_ticks))
          
          if (!is.null(sec$stn_x)) {
            stn_labels = if (!is.null(sec$stn_names)) sec$stn_names else FALSE
            axis(3, at = sec$stn_x, labels = stn_labels, cex.axis = 0.7)
          }
          
          tryCatch({
            contour(x_vec, y_vec, z_plot, add = TRUE, col = adjustcolor("white", alpha.f = 0.4))
          }, error = function(e) NULL)
          
          plots_drawn = plots_drawn + 1 # Log success
          
        } else {
          message(paste("Skipping", field_name, "in Transect", i, "- non-finite range."))
        }
      } else {
        message(paste("Skipping", field_name, "in Transect", i, "- no valid data points."))
      }
      
    }, error = function(e) {
      # If a crash happens, print it but KEEP GOING so the file doesn't lock
      message(paste("CRASH AVOIDED on", field_name, "in Transect", i, ":", e$message))
    })
  }
  
  # 3. Prevent 0-page corrupted file errors
  if (plots_drawn == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = paste("No Data for Transect", i))
    text(1, 1, "All variables were skipped or failed to plot.", cex = 1.2)
  }
  
  dev.off() # Always reached safely now
}

browseURL(paste0(save.dir, dataset_name, ' Section Plots (transect 1).pdf'))