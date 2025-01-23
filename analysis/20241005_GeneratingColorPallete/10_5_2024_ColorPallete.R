library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)

# Set up file paths
outPath = "analysis/out" # out
referenceASV = "config/referenceASVs-e0026.txt" #reference ASVs
KCHpalette_path = "config/KCHcolors-Silva-partial.txt" #color palette
datae0058meta = "analysis/out/datae0058meta.txt" #metadata e0058
datae0059meta = "analysis/out/datae0059meta.txt" #metadata e0059

# Load the color palette
KCHpalette <- read.table(KCHpalette_path, header = TRUE, stringsAsFactors = FALSE)

# Function to process metadata and create color palette
process_metadata <- function(metadata_path, outPath) {
  # Load metadata
  metadata <- read.table(metadata_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Create a stripped-down color palette containing only the families present in the dataset
  metadata <- metadata %>%
    mutate(fullSilvataxonomy = paste(Kingdom, Phylum, Class, Order, Family, sep = "."))
  
  KCHpalette_filtered <- KCHpalette %>%
    filter(taxa %in% sort(unique(metadata$fullSilvataxonomy))) %>%
    mutate(taxashort = ifelse(taxashort == "", gsub(".*\\.", "", taxa), taxashort))
  
  # Make a named list
  KCHpalette_filtered_vector <- KCHpalette_filtered$hex
  names(KCHpalette_filtered_vector) <- KCHpalette_filtered$taxashort
  
  # Save the color palette vector
  saveRDS(KCHpalette_filtered_vector, file = paste0(outPath, "/KCHpalette_filtered_vector.rds"))
  
  # Return as a list
  return(list(metadata = metadata, palette_vector = KCHpalette_filtered_vector))
}

# Process both datasets
results_e0058 <- process_metadata(datae0058meta, outPath)
results_e0059 <- process_metadata(datae0059meta, outPath)

# Save Palette Vectors:
#e0058
metadata_e0058 <- results_e0058$metadata
KCHpalette_vector_e0058 <- results_e0058$palette_vector
saveRDS(KCHpalette_vector_e0058, file = paste0(outPath, "/KCHpalette_vector_e0058.rds"))
#e0059
metadata_e0059 <- results_e0059$metadata
KCHpalette_vector_e0059 <- results_e0059$palette_vector
saveRDS(KCHpalette_vector_e0059, file = paste0(outPath, "/KCHpalette_vector_e0059.rds"))

