# ###
#
# Motility modulator analysis
# By: Ajay Bhargava
# 07/13/2020
#
# ##

# Scaling
scale <- 0.4023

# Internal FX
'%!in%' <- function(x,y)!('%in%'(x,y))

# Plotting Tools
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/motility-modulator-analysis/')
library(tidyverse)
library(ggpubr)
library(gridExtra)
source('./src/R-tools/graphing_theme.R')


# Read in each dataset
RDS.ONE <- readRDS('../../shared-assets-local/Distribution-Dataset-14433144.rds')
RDS.TWO <- readRDS('../../shared-assets-local/EdU-Coordinates-Dataset-14433144.rds')
RDS.THREE <- readRDS('../../shared-assets-local/Spatial-Hedgemony-Dataset-14433144.rds')

# #######
#
# Process RDS.ONE -> data.one
# Process the subclone size distribution such that a comparison between "CTRL", "TGFBE", "TGFBL", and "SMIFH2" can be made.
#
# #######

# For ECDF
data.one <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size") %>%
            filter(Treatment %in% c("CTRL", "TGFBE", "TGFBL", "SMIFH2")) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size)

# For Bar Plot of Size Deviation
data.two <- data.one %>%
            group_by(Treatment, Colony.ID) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size)) %>%
            replace_na(list(Var = 0, SD = 0)) %>%
            ungroup()

# Plot One of Subclone Size Deviation across treatments
plot.one <- data.two %>%
            ggplot(aes(x = Treatment, y = SD)) +
            stat_summary(geom = "bar",
                        color = "black",
                        fill = "white",
                        linetype = "solid",
                        size = 1,
                        fun.y = "mean") +
            theme_publication() +
            stat_summary(geom = "errorbar",
                        width = 0.1,
                        fun.ymax = function(x) mean(x) + sd(x) / sqrt(length(x)),
                        fun.ymin = function(x) mean(x) - sd(x) / sqrt(length(x))) +
            stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE"), c("CTRL", "TGFBL"), c("TGFBE", "TGFBL")), size = 4, symnum.args = symnum.args) +
            labs(x = "Condition", y = "Subclone Size Distribution Standard Deviation")

# Plot two of ECDF by treatment
plot.two <- data.one %>%
            ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            annotation_logticks(sides = "b")

# #######
#
# Process RDS.TWO -> data.three
# Process the subclone size distribution such that a comparison between "CTRL", "TGFBE", "TGFBL", and "SMIFH2" can be made.
#
# #######

data.three <- RDS.TWO %>%
              select("N", "Colony.ID", "Treatment", "Colony.Size", "Centroid.X", "Centroid.Y", "D.Free", "D.Well", "X", "Y") %>%
              filter(Treatment %in% c("CTRL", "TGFBE", "TGFBL", "SMIFH2")) %>%
              mutate(D.Centroid.to.EdU = sqrt((X - Centroid.X)^2 + (Y - Centroid.Y)^2)) %>%
              mutate(EdU.Free = (D.Free) / (D.Free +  D.Centroid.to.EdU)) %>%
              mutate(EdU.Well = (D.Well) / (D.Well +  D.Centroid.to.EdU)) %>%
              mutate(EdU.Class = if_else(D.Well < 100 & D.Free < 100, "Confound", if_else(D.Well > 100 & D.Free < 100, "Free", if_else(D.Well < 100 & D.Free > 100, "Boundary", if_else(D.Well > 100 & D.Free > 100, "Inside", "Not-Sure"))))) %>%
              group_by(Treatment) %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
              ungroup() %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              select(N, Treatment, Colony.ID, Colony.Size, EdU.Free, EdU.Well, EdU.Class, Size) %>%
              as_tibble()

# Colonies that don't touch a boundary
data.four <- data.three %>% filter(is.na(EdU.Class)) %>% select(-c(EdU.Well, EdU.Class))

# Colonies that do touch a boundary
data.five <- data.three %>% filter(!is.na(EdU.Class)) %>% filter(EdU.Class %!in% c("Inside", "Confound"))

# Plot three (Distribution of EDU points by Treatment for colonies that do not )

plot.three <- data.four %>%
              ggplot(aes(y = EdU.Free, x = Treatment, fill = Treatment)) +
              geom_violin(lwd = 1.1) +
              theme_publication() +
              labs(x = "Treatment", y = "EdU+'ve cell position \n (Normalized)")
