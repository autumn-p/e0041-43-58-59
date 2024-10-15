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

# Define community types
community_types <- c("donor_only", "recipient_only", "donor+recipient", "super+recipient")

# Create and save community plots
for (type in community_types) {
  create_community_plot(datae0059meta, type, outPath)
}

# Function to create and save alpha diversity plots
create_alpha_diversity_plot <- function(data, community_type, outPath) {
  plot <- data %>%
    filter(community_mixture == community_type) %>%
    ggplot() +
    geom_boxplot(aes(x = community_mixture, y = alpha_diversity, fill = community_mixture), color = "black") +
    geom_jitter(aes(x = community_mixture, y = alpha_diversity), width = 0.2, height = 0, color = "blue", size = 1.5) +
    labs(x = "Community Mixture", y = "Alpha Diversity", 
         title = paste("Alpha Diversity for", community_type, "Communities")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  save_plot(paste0(outPath, "/", community_type, "_alpha_diversity_plot.png"), plot, base_width = 10, base_height = 10)
  
  return(plot)
}

# Function to compare communities for a given donor
compare_communities <- function(donor_id, data) {
  full_community <- data %>%
    filter(donor == donor_id & recipient == "full-community")
  
  other_communities <- data %>%
    filter(donor == donor_id & recipient != "full-community")
  
  full_community_summary <- full_community %>%
    group_by(Family) %>%
    summarize(number_of_colonizers = n(), .groups = 'drop')
  
  plots <- list()
  
  for (recip in unique(other_communities$recipient)) {
    recip_community <- other_communities %>%
      filter(recipient == recip)
    
    recip_summary <- recip_community %>%
      group_by(Family) %>%
      summarize(number_of_colonizers = n(), .groups = 'drop')
    
    community_diff <- full_community_summary %>%
      full_join(recip_summary, by = "Family", suffix = c("_full", paste0("_", recip))) %>%
      mutate(difference = coalesce(number_of_colonizers_full, 0) - coalesce(get(paste0("number_of_colonizers_", recip)), 0))
    
    plot <- community_diff %>%
      ggplot(aes(x = Family, y = difference, fill = Family)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = KCHpalettee0059vector) +
      labs(title = paste("Difference in Colonization: Full vs", recip), 
           x = "Family", 
           y = "Difference in Number of Colonizers (Full -", recip, ")") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    plots[[recip]] <- plot
    
    save_plot(paste0(outPath, "/community_diff_plot_", donor_id, "_", recip, ".png"), plot, base_width = 15, base_height = 10)
  }
  
  return(plots)
}

# Compare communities for all unique donors
unique_donors <- unique(datae0059meta$donor)
all_plots <- lapply(unique_donors, compare_communities, data = datae0059meta)
