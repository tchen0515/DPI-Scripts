#' @export
load.acs.files = function(files, dt = 1, min.size = 1024, verboes = T) {
  
  #### Filter files by size
  file = data.frame(files = files, keep = T)
  
  ## Remove files smaller than 1kB
  for (i in 1:nrow(file)) {
    if (file.size(file$file[i]) < min.size) {
      file$keep[i] = F
    }
  }
  
  message(' Identified ', sum(file$keep), ' file(s) of valid size and ', sum(!file$keep), ' file(s) of invalid size.')
  files = file$files[file$keep]
  
  if (length(files) == 0) {
    stop("No valid files found.")
  }
  
  ## Helper to process timestamp calculation safely
  process_acs_time = function(df, start.time) {
    # Extract millisecond column safely and force numeric conversion
    raw_col = if ("Time(ms)" %in% colnames(df)) df$`Time(ms)` else df[[1]]
    raw_time = as.numeric(raw_col)
    
    # Calculate datetime
    df$Time = (raw_time - min(raw_time, na.rm = TRUE)) / 1000 + start.time
    
    # Remove original millisecond column
    if ("Time(ms)" %in% colnames(df)) {
      df$`Time(ms)` = NULL
    } else {
      df[[1]] = NULL
    }
    return(df)
  }
  
  ## Load in first file.
  message(' Attempting to load file 1... ', appendLF = F)
  stamp = strsplit(x = files[1], split = '/')[[1]]
  stamp = stamp[length(stamp)]
  stamp = strsplit(stamp, '_')[[1]]
  stamp = gsub('.dat', '', stamp[length(stamp)])
  
  start.time = as.POSIXct(substr(stamp, 1, 14), format = "%Y%m%d%H%M%S", tz = 'UTC')
  
  # FIX: Skip dynamically to the header line starting with "Time(ms)"
  dat = as.data.frame(data.table::fread(files[1], skip = "Time(ms)", verbose = F, showProgress = F))
  dat = process_acs_time(dat, start.time)
  message('Done.')
  
  ## Read in rest of data
  if (length(files) > 1) {
    for (i in 2:length(files)) {
      message(' Attempting to load file ', i, '... ', appendLF = F)
      temp = as.data.frame(data.table::fread(files[i], skip = "Time(ms)", verbose = F, showProgress = F))
      
      ## Time
      stamp = strsplit(x = files[i], split = '/')[[1]]
      stamp = stamp[length(stamp)]
      stamp = strsplit(stamp, '_')[[1]]
      stamp = gsub('.dat', '', stamp[length(stamp)])
      
      start.time = as.POSIXct(substr(stamp, 1, 14), format = "%Y%m%d%H%M%S", tz = 'UTC')
      temp = process_acs_time(temp, start.time)
      
      if (ncol(temp) != ncol(dat)) {
        message('Invalid number of columns (', ncol(temp), ' vs ', ncol(dat), '), skipping.')
      } else {
        dat = rbind.data.frame(dat, temp)
        message('Done.')
      }
    }
  }
  
  message('Finished.')
  dat
}