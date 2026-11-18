## (Both Trian & val) summarize the confusion matrix , combining same taxonomic category
setwd('F:/DPI/')
modeln <- 'CNXcce4'

# 1. Define the summarization logic as a reusable function
summarize_taxonomy <- function(input_path, output_path) {
  # Read the CSV
  confusion <- read.csv(input_path) 
  categories <- confusion[,1]
  confusion <- confusion[,-1]
  
  # Split strings to get the major taxonomic category
  tmp <- strsplit(categories, split = '_')
  maj <- c()
  for (i in 1:length(tmp)) {
    maj <- c(maj, tmp[[i]][1])
  }
  
  major_unique <- unique(maj)
  
  # Initialize the summary matrix
  sumconfusion <- array(NA, dim = rep(length(major_unique), 2))
  rownames(sumconfusion) <- major_unique
  colnames(sumconfusion) <- major_unique
  
  # Aggregate the counts
  for (i in 1:nrow(sumconfusion)) {
    for (j in 1:ncol(sumconfusion)) {
      # Use exact matching (==) rather than grepl to avoid substring bugs
      l1 <- maj == major_unique[i]
      l2 <- maj == major_unique[j]
      
      sumconfusion[i,j] <- sum(confusion[l1, l2])
    }
  }
  
  # Export the summarized file
  write.csv(sumconfusion, file = output_path)
  cat("Processed and saved:", output_path, "\n")
}

# 2. Process the Validation Confusion Matrix
val_in <- paste0('model/', modeln, ' confusion.csv')
val_out <- paste0('model/', modeln, ' sumconfusion.csv')
summarize_taxonomy(val_in, val_out)

# 3. Process the Training Confusion Matrix
train_in <- paste0('model/', modeln, '_train_confusion.csv')
train_out <- paste0('model/', modeln, '_train_sumconfusion.csv')
summarize_taxonomy(train_in, train_out)