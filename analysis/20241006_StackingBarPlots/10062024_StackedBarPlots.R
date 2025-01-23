library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)

# Set up file paths
outPath = "analysis/20241006_StackingBarPlots/out" # out
datae0058meta_path = "analysis/09102024_ModifyingMetaDataTables/out/datae0058meta.txt" #e0058 metadata file
datae0059meta_path = "analysis/09102024_ModifyingMetaDataTables/out/datae0059meta.txt" #e0059 metadata file


# Read in palettes and metadata tables
KCHpalettee0058vector <- readRDS("analysis/20241005_GeneratingColorPallete/out/KCHpalette_vector_e0058.rds") #e0058 color palette
KCHpalettee0059vector <- readRDS("analysis/20241005_GeneratingColorPallete/out/KCHpalette_vector_e0059.rds") #e0059 color palette
datae0058meta <- read.table(datae0058meta_path, header = TRUE, stringsAsFactors = FALSE)
datae0059meta <- read.table(datae0059meta_path, header = TRUE, stringsAsFactors = FALSE)

# Function to create stacked bar plots
create_stacked_bar_plot <- function(data, palette_vector, facet_vars, output_filename, x_label, y_label, plot_title) {
  # Create the stacked bar plot
  plot <- data %>% 
    ggplot() +
    geom_bar(aes(y = relAbundance, x = sample, fill = factor(Family)), color = "black", stat = "identity") +
    scale_fill_manual(values = palette_vector) +
    facet_wrap(as.formula(paste("~", facet_vars)), scales = "free_x", ncol = 12) +
    scale_x_discrete(breaks = NULL) +
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(plot_title) 
  
  # Save the plot
  save_plot(paste0(outPath, "/", output_filename), plot, base_width = 25, base_height = 40)
  
  return(plot)
}

# Create and save e0058 stacked bar plot
create_stacked_bar_plot(datae0058meta, KCHpalettee0058vector, "well + recipient", "e0058communityAbundanceBarByCommunity.png", "Recipient", "Relative Abundance", "Distribution of Family Relative Abundance by Well")

# Create and save e0059 stacked bar plot
create_stacked_bar_plot(datae0059meta, KCHpalettee0059vector, "plate + well", "e0059communityAbundanceBarByCommunity.png", "Mixture", "Relative Abundance", "Distribution of Family Relative Abundance by Well")
