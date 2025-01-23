library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)

# Set up file paths
outPath = "analysis/20241008_AddingCommunityMixtureData/out" # out
datae0041meta_path = "analysis/09102024_ModifyingMetaDataTables/out/datae0041meta.txt" #e0041 metadata file
datae0058meta_path = "analysis/09102024_ModifyingMetaDataTables/out/datae0058meta.txt" #e0058 metadata file
datae0059meta_path = "analysis/09102024_ModifyingMetaDataTables/out/datae0059meta.txt" #e0059 metadata file

# Read in the metadata tables
datae0041meta <- read.table(datae0041meta_path, header = TRUE, stringsAsFactors = FALSE)
datae0058meta <- read.table(datae0058meta_path, header = TRUE, stringsAsFactors = FALSE)
datae0059meta <- read.table(datae0059meta_path, header = TRUE, stringsAsFactors = FALSE)

# Create the `community_mixture` column
prepare_data <- function(data) {
  data %>%
    mutate(community_mixture = case_when(
      donor == "super-community" & recipient != "blank" ~ "super+recipient",
      donor == "blank" & recipient != "blank" ~ "recipient_only",
      donor != "blank" & recipient == "blank" ~ "donor_only",
      donor != "blank" & donor != "super-community" & recipient != "blank" ~ "donor+recipient",
      TRUE ~ NA_character_
    ))
}
datae0041meta <- prepare_data(datae0041meta)

## Save tables with metadata to out folder
write.table(datae0041meta, paste0(outPath, "/datae0041meta_comm_mix_1_23_25.txt"), row.names = FALSE, quote = FALSE)

