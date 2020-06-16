# ###
#
# In-Vitro Analysis of Boundary and Well EdU Distribution
# By: Ajay Bhargava
# 06/15/2020
#
# ##


# Plotting Criteria
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/in-vitro-edu-analysis/')
library(tidyverse)
library(ggpubr)
library(gridExtra)
source('./src/R-tools/graphing_theme.R')

# Load Data and Normalize Distances to microscopy reference

df <- readRDS('../../shared-assets-local/EdU-Analysis-Output.rds')
df$Colony.Size <- df$Colony.Size / 0.4023
df$D.Free <- df$D.Free / 0.4023
df$D.Well <- df$D.Well /0.4023

# Create Boundary EdU class and get distances to centroid

df.one <- df %>%
          mutate(D.Centroid.to.EdU = sqrt((X - Centroid.X)^2 + (Y - Centroid.Y)^2)) %>%
          mutate(EdU.Free = (D.Free) / (D.Free +  D.Centroid.to.EdU)) %>%
          mutate(EdU.Well = (D.Well) / (D.Well +  D.Centroid.to.EdU)) %>%
          mutate(EdU.Type = if_else(D.Well < 50, 'Boundary', 'Free-Edge')) %>%
          mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
          mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
          select(N, Colony.ID, EdU.Free, EdU.Well, EdU.Type, Size) %>%
          as_tibble()

# Data One is Colonies that don't touch a boundary

data.one <- df.one %>% filter(is.na(EdU.Type)) %>% select(-c(EdU.Well, EdU.Type))

# Data Two is Colonies that do touch a boundary

data.two <- df.one %>% filter(!is.na(EdU.Type))
