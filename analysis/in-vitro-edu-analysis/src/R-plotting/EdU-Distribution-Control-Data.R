# ###
#
# In-Vitro Analysis of Boundary and Well EdU Distribution - KPCre Cells
# By: Ajay Bhargava
# 06/15/2020
#
# ##


# Plotting Criteria
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/in-vitro-edu-analysis/')
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(scales)
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
          mutate(EdU.Class = if_else(D.Well < 100 & D.Free < 100, "Confound", if_else(D.Well > 100 & D.Free < 100, "Free", if_else(D.Well < 100 & D.Free > 100, "Boundary", if_else(D.Well > 100 & D.Free > 100, "Inside", "Not-Sure"))))) %>%
          mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
          mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
          select(N, Colony.ID, Colony.Size, EdU.Free, EdU.Well, EdU.Class, Size) %>%
          as_tibble()

# Data One is Colonies that don't touch a boundary

data.one <- df.one %>% filter(is.na(EdU.Class)) %>% select(-c(EdU.Well, EdU.Class))

# Data Two is Colonies that do touch a boundary

data.two <- df.one %>% filter(!is.na(EdU.Class)) %>% filter(EdU.Class %!in% c("Inside", "Confound"))

# Plot One/Two - Distribution of EdU Points for Colonies that don't touch a boundary (paired with pictures), and description of tumor sizes.

plot.one <- data.one %>%
            ggplot(aes(x = EdU.Free, fill = Size)) +
            geom_density(color = 'black', alpha = 0.4) +
            theme_publication() +
            labs(x = "EdU Distance from Boundary", y = "Density") +
            theme(legend.position = 'none')

plot.two <- data.one %>%
            group_by(Size) %>%
            summarize(mean =  mean(Colony.Size), sd = sd(Colony.Size)) %>%
            ggplot(aes(x = Size, y = mean)) +
            geom_col(position = 'dodge', color = 'black') +
            theme_publication() +
            theme(legend.position = 'none') +
            labs(x = "Tumor Group", y = "Tumor Size") +
            geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, position = 'dodge') +
            scale_y_continuous(labels = scientific,  limits = c(NA, 8000000))

# Plot Three - Measure of the average # of EdU points for colonies facing the well vs facing the growth zone for the same colony

plot.three <- data.two %>%
              group_by(Colony.ID, EdU.Class) %>%
              tally() %>%
              ggplot(aes(x = EdU.Class, y = n)) +
              geom_boxplot(color = 'black', lwd = 1.1) +
              theme_publication() +
              labs(x = "EdU Location", y = "Number of EdU+'ve cells") +
              stat_compare_means(comparisons = list(c("Free", "Boundary")), size = 4, method = 't.test', symnum.args = symnum.args)

ggsave('./reports/figures/Plot-One.eps', plot.one, width = 4, height = 4, device=  "eps")
ggsave('./reports/figures/Plot-Two.eps', plot.two, width = 4, height = 4, device=  "eps")
ggsave('./reports/figures/Plot-Three.eps', plot.three, width = 4, height = 4, device=  "eps")

# Calculate properties for plotting - density peak for plot one.
