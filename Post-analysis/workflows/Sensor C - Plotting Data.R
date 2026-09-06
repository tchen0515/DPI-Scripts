library(viridis)
library(ggplot2)
library(maps)
library(mgcv)
library(fields)

## -----------------------------------------------------------------------------
## SETUP & CONFIGURATION (Only change dataset_name when switching datasets)
## -----------------------------------------------------------------------------
dataset_name = "cycle1_day1"

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

# 1. Combine all transects into a single dataframe to prevent ggplot loop errors
all_transects_list = lapply(seq_along(transects), function(i) {
  df = transects[[i]]
  df$Transect = as.factor(i)
  return(df)
})
all_transects = do.call(rbind, all_transects_list)

# 2. Dynamically calculate geographic bounding box from actual data limits
lon_min = min(all_transects$Longitude, na.rm = TRUE) - 0.5
lon_max = max(all_transects$Longitude, na.rm = TRUE) + 0.5
lat_min = min(all_transects$Latitude, na.rm = TRUE) - 0.5
lat_max = max(all_transects$Latitude, na.rm = TRUE) + 0.5

# Calculate midpoint coordinates for transect labels
transect_labels = aggregate(cbind(Longitude, Latitude) ~ Transect, data = all_transects, FUN = mean, na.rm = TRUE)

# 3. Overview Map (All Transects)
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

# 4. Individual Transect Maps
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
    vals = df_i[[j]]
    var_name = names(df_i)[j]
    
    if (is.numeric(vals) && !all(is.na(vals)) && !var_name %in% c("Time", "Distance", "Depth")) {
      val_range = range(vals, na.rm = TRUE)
      val_cut = cut(vals, breaks = 100)
      col_vec = viridis::inferno(100)[as.numeric(val_cut)]
      
      # Expand right margin to make room for color bar legend
      par(mar = c(4, 4, 3, 5))
      
      plot(df_i$Latitude, df_i$Depth,
           pch = 16, cex = 0.5,
           col = col_vec,
           ylim = c(100, 0),
           xlab = "Latitude", ylab = "Depth (m)",
           main = paste(var_name, "- Transect", i))
      
      # Draw side color bar legend
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
  grid_x = seq(min(df$Distance, na.rm = TRUE), max(df$Distance, na.rm = TRUE), length.out = nx)
  grid_y = seq(ylim[1], ylim[2], length.out = ny)
  pred_grid = expand.grid(Distance = grid_x, Depth = grid_y)
  
  grid_list = list()
  
  for (f in fields) {
    valid = !is.na(df$Distance) & !is.na(df$Depth) & !is.na(df[[f]])
    if (sum(valid) >= 10) {
      mod = mgcv::gam(df[[f]][valid] ~ s(Distance, Depth, k = min(30, sum(valid) - 1)), data = df[valid, ])
      grid_mat = matrix(predict(mod, newdata = pred_grid), nrow = nx, ncol = ny)
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
## 4. PLOT SECTIONS (With Image Color Bar)
## -----------------------------------------------------------------------------
for (i in seq_along(section)) {
  section_pdf_path = paste0(save.dir, dataset_name, ' Section Plots (transect ', i, ').pdf')
  pdf(section_pdf_path, width = 10, height = 8)
  par(mfrow = c(2, 1))
  
  sec = section[[i]]
  for (field_name in names(sec$grid)) {
    z_mat = sec$grid[[field_name]]
    if (!all(is.na(z_mat))) {
      # fields::image.plot automatically includes side color scale
      fields::image.plot(sec$x, sec$y, z_mat,
                         ylim = c(200, 0),
                         col = viridis::inferno(100),
                         xlab = 'Section Distance (km)',
                         ylab = 'Depth (m)',
                         main = paste0(dataset_name, ' - Transect ', i, ' - ', field_name))
      
      contour(sec$x, sec$y, z_mat, add = TRUE, col = adjustcolor("white", alpha.f = 0.4))
    }
  }
  
  dev.off()
}

browseURL(paste0(save.dir, dataset_name, ' Section Plots (transect 1).pdf'))