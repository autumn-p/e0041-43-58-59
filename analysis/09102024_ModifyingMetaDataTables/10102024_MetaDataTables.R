library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)

# Set up file paths
outPath = "analysis/09102024_ModifyingMetaDataTables/out" # out
e0058dataframePath = "e0058-59_data/e0058ps_all.txt.gz" #raw data e0058
e0058appendCol_path = "e0058-59_data/e0058metadata.tsv" #metadata e0058 
e0059dataframePath = "e0058-59_data/e0059ps_all.txt.gz" #raw data e0059
e0059appendCol_path = "e0058-59_data/e0059metadata.tsv" #metadata e0059
referenceASV = "config/referenceASVs-e0026.txt"
e0041dataframePath = "e0040-41-43_data/ps_all.txt.gz" #raw data e0058
e0041appendCol_path = "e0040-41-43_data/metadatae0041.tsv" #metadata e0041

# Function to process a dataset
process_dataset <- function(dataset_path, metadata_path) {
  # Read in dataframe
  data <- read.table(dataset_path, header = TRUE, stringsAsFactors = FALSE)
  # Import metadata table
  appendCol <- read.table(metadata_path, header = TRUE)
  # Join the metadata table to the original data frame
  data_meta <- left_join(data, appendCol)
  
  # Filter the data frame to include only rows with relAbundance greater than 0.1%
  data_meta <- data_meta %>% filter(relAbundance > 0.001)
  
  # Get alpha diversity by sample w/ limit of detection
  alpha_diversity <- data_meta %>%
    group_by(sample) %>%
    summarize(alpha_diversity = sum(count > 0))
  
  # Join alpha_diversity to the original table
  data_meta <- data_meta %>%
    left_join(alpha_diversity, by = c('sample'))
  
  return(data_meta)
}

# Process both datasets
datae0041meta <- process_dataset(e0041dataframePath, e0041appendCol_path)
datae0058meta <- process_dataset(e0058dataframePath, e0058appendCol_path)
datae0059meta <- process_dataset(e0059dataframePath, e0059appendCol_path)

# Save tables with metadata to out folder
write.table(datae0041meta, paste0(outPath, "/datae0041meta.txt"), row.names = FALSE, quote = FALSE)
write.table(datae0058meta, paste0(outPath, "/datae0058meta.txt"), row.names = FALSE, quote = FALSE)
write.table(datae0059meta, paste0(outPath, "/datae0059meta.txt"), row.names = FALSE, quote = FALSE)
