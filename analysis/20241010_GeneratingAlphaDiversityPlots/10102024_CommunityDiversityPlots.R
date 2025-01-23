library(gridExtra)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)

# Set up file paths
outPath = "analysis/out" # out
datae0058meta_path = "analysis/out/datae0058meta.txt" #e0058 metadata file
datae0059meta_path = "analysis/out/datae0059meta_comm_mix.txt" #e0059 metadata file w/comm mix type

# Read in the metadata tables and color palettes
datae0058meta <- read.table(datae0058meta_path, header = TRUE, stringsAsFactors = FALSE)
datae0059meta <- read.table(datae0059meta_path, header = TRUE, stringsAsFactors = FALSE)
KCHpalettee0058vector <- readRDS("analysis/out/KCHpalette_vector_e0058.rds") #e0058 color palette
KCHpalettee0059vector <- readRDS("analysis/out/KCHpalette_vector_e0059.rds") #e0059 color palette

# Function to create community diversity plots
create_community_plot <- function(data, community_type, outPath) {
  plot <- data %>%
    filter(community_mixture == community_type) %>%
    ggplot() +
    geom_bar(aes(x = sample, y = relAbundance, fill = factor(Family)), color = "black", stat = "identity") +
    scale_fill_manual(values = KCHpalettee0059vector) +
    labs(x = "Sample", y = "Relative Abundance", 
         title = paste("Relative Abundance Distribution of", community_type, "Communities")) +
    scale_x_discrete(breaks = NULL)
  
  save_plot(paste0(outPath, "/", community_type, "_plot.png"), plot, base_width = 10, base_height = 10)
  
  return(plot)
}

# Function to create and save alpha diversity plots
create_alpha_diversity_plot <- function(data, community_type, outPath) {
  plot <- data %>%
    filter(community_mixture == community_type) %>%
    ggplot() +
    geom_boxplot(aes(x = community_mixture, y = alpha_diversity_e0059, fill = community_mixture), color = "black") +
    geom_jitter(aes(x = community_mixture, y = alpha_diversity_e0059), width = 0.2, height = 0, color = "blue", size = 1.5) +
    labs(x = "Community Mixture", y = "Alpha Diversity", 
         title = paste("Alpha Diversity for", community_type, "Communities")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  save_plot(paste0(outPath, "/", community_type, "_alpha_diversity_plot.png"), plot, base_width = 10, base_height = 10)
  
  return(plot)
}


# Fill in microbial_data & community_type to get plots and compare
# Call the function to create and save the community diversity plot
community_plot <- create_community_plot(datae0059meta, "donor_only", outPath)
# Call the function to create and save the alpha diversity plot
alpha_diversity_plot <- create_alpha_diversity_plot(microbial_data, community_type, outPath)
