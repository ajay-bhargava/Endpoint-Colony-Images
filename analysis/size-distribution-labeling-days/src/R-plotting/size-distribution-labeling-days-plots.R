# ###
#
# Ex-vivo analysis of subclone size distribution to create an ECDF chart to then compare between the 3D and 2D case.
# By: Ajay Bhargava
# 05/31/2020
#
# ##

# Plotting Tools
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/')
library(tidyverse)
library(ggpubr)
library(gridExtra)
source('./analysis/size-distribution-labeling-days/src/R-functions/graphing-theme-publication.R')

# Read Data
df <-  readRDS('./shared-assets-local/Size-Distribution-Labeling-Days-Dataset.rds')
scale <- 0.4023
df$Colony.Size <- df$Colony.Size / scale
df$D <- df$D / scale
df$Subclone.Size <- df$Subclone.Size / scale

# Create Data.One
data.one <- df %>%
            subset(Subclone.Color != "CFP") %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size, Subclone.Size.Cells = floor(Subclone.Size / 525)) %>%
            filter(Subclone.Size.Cells != 0)

# Create Data.Two
data.two <- data.one %>%
            subset(Subclone.Color != "CFP") %>%
            group_by(Condition, Colony.ID) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size)) %>%
            replace_na(list(Var = 0, SD = 0)) %>%
            ungroup()

# Create Plot that looks at subclone size deviation
plot.one <- data.two %>%
            mutate(Cond.Name = fct_relevel(Condition, "DAY03", "DAY10", "DAY15")) %>%
            ggplot(aes(x = Cond.Name, y = SD)) +
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
            stat_compare_means(comparisons = list(c("DAY03", "DAY15"), c("DAY03", "DAY10"), c("DAY10", "DAY15")), size = 4, method = 't.test', symnum.args = symnum.args) +
            labs(x = "Condition", y = "Subclone Size Distribution Standard Deviation")

# Create ECDF that looks at all conditions and shows the difference in the size distribution between the different labeling days, thus corroborating the data from the in-vivo scenario
# This is to create a series of lines that will denote the places where the maximum D is acheived, in illustrator, these lines will be truncated to form the segments.  Furthermore p-vlaue will be plotted.
cdf.day.three <- ecdf(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size)
cdf.day.ten <- ecdf(data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size)
cdf.day.fifteen <- ecdf(data.one %>% filter(Condition == "DAY15") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size)

minMax.three.ten <- seq(min(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), max(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), length.out=length(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size))
minMax.three.fifteen <- seq(min(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY15") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), max(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY15") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), length.out=length(data.one %>% filter(Condition == "DAY03") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size))
minMax.ten.fifteen <- seq(min(data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY15") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), max(data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size, data.one %>% filter(Condition == "DAY15") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size), length.out=length(data.one %>% filter(Condition == "DAY10") %>% select(Norm.Subclone.Size) %>% .$Norm.Subclone.Size))

x0.three.ten <- minMax.three.ten[which(abs(cdf.day.three(minMax.three.ten) - cdf.day.ten(minMax.three.ten)) == max(abs(cdf.day.three(minMax.three.ten) - cdf.day.ten(minMax.three.ten))))]
x0.three.fifteen <- minMax.three.fifteen[which(abs(cdf.day.three(minMax.three.fifteen) - cdf.day.fifteen(minMax.three.fifteen)) == max(abs(cdf.day.three(minMax.three.fifteen) - cdf.day.fifteen(minMax.three.fifteen))))]
x0.ten.fifteen <- minMax.ten.fifteen[which(abs(cdf.day.ten(minMax.ten.fifteen) - cdf.day.fifteen(minMax.ten.fifteen)) == max(abs(cdf.day.ten(minMax.ten.fifteen) - cdf.day.fifteen(minMax.ten.fifteen))))]


## Need to create the ECDF for this data
plot.two <- data.one %>%
            ggplot(aes(Norm.Subclone.Size, color = Condition)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            theme(legend.position = 'none') +
            annotation_logticks(sides = "b") +
            geom_vline(xintercept = x0.three.ten, linetype = 'dashed') +
            geom_vline(xintercept = x0.three.fifteen, linetype = 'solid') +
            geom_vline(xintercept = x0.ten.fifteen, linetype = 'twodash')


# Save Plots
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-One.eps', plot = plot.one, width = 4, height = 6, device = "eps")
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Two.eps', plot = plot.two, width = 4, height = 6, device = "eps")

# Create ECDF that looks at the comparison of YFP and dTomato subclones over time

plot.three <- data.one %>%
            filter(Condition == "DAY03") %>%
            ggplot(aes(Norm.Subclone.Size, color = Subclone.Color)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_color_manual(values=c('#FF6347','#E69F00')) +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            annotation_logticks(sides = "b") +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            theme(legend.position = 'none')
            # geom_vline(aes(xintercept=quartiles.colors.day.three[4]),linetype = "dashed")


plot.four <- data.one %>%
            filter(Condition == "DAY10") %>%
            ggplot(aes(Norm.Subclone.Size, color = Subclone.Color)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_color_manual(values=c('#FF6347','#E69F00')) +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            annotation_logticks(sides = "b") +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            theme(legend.position = 'none')
            # geom_vline(aes(xintercept=quartiles.colors.day.ten[4]),linetype = "dashed")

plot.five <- data.one %>%
            filter(Condition == "DAY15") %>%
            ggplot(aes(Norm.Subclone.Size, color = Subclone.Color)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_color_manual(values=c('#FF6347','#E69F00')) +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            annotation_logticks(sides = "b") +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            theme(legend.position = 'none')
            # geom_vline(aes(xintercept=quartiles.colors.day.fifteen[4]),linetype = "dashed")

# Create a bar graph that looks at the number of times that dTomato or yPET was the largest subclone for each day.
data.three <- data.one %>%
              select(N, Colony.ID, Condition, Subclone.Size, Subclone.Color, Subclone.Size.Cells) %>%
              group_by(Colony.ID) %>%
              filter(Subclone.Size.Cells  == max(Subclone.Size.Cells))

# Graph the summary statistics (as a bar graph) for the data
plot.six <- data.three %>%
             group_by(Condition, N, Subclone.Color) %>%
             tally() %>%
             ungroup() %>%
             group_by(Subclone.Color) %>%
             summarize(mean = mean(n), sd = sd(n), n = n()) %>%
             ggplot(aes(x = Subclone.Color, y = n, fill = Subclone.Color)) +
             geom_col(position = 'dodge', color = 'black') +
             theme_publication() +
             scale_fill_manual(values=c('#FF6347','#E69F00')) +
             theme(legend.position = 'none') +
             labs(x = "Condition", y = "Identity of Largest Subclone") +
             geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2, position = 'dodge')

# > compare_means(n ~ Subclone.Color, data.three %>%
#               group_by(Condition, N, Subclone.Color) %>%
#               tally() %>%
#               ungroup() %>%
#               group_by(Subclone.Color))
# # A tibble: 1 x 8
#  .y.   group1  group2      p p.adj p.format p.signif method
#  <chr> <chr>   <chr>   <dbl> <dbl> <chr>    <chr>    <chr>
# 1 n     dTomato yPET   0.0174 0.017 0.017    *        Wilcoxon
#
# > compare_means(n ~ Subclone.Color, data.three %>%
#               group_by(Condition, N, Subclone.Color) %>%
#               tally() %>%
#               ungroup() %>%
#               group_by(Subclone.Color), method = 't.test')
# # A tibble: 1 x 8
#  .y.   group1  group2       p  p.adj p.format p.signif method
#  <chr> <chr>   <chr>    <dbl>  <dbl> <chr>    <chr>    <chr>
# 1 n     dTomato yPET   0.00523 0.0052 0.0052   **       T-test

ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Three.eps', plot = plot.three, width = 4, height = 6, device = "eps")
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Four.eps', plot = plot.four, width = 4, height = 6, device = "eps")
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Five.eps', plot = plot.five, width = 4, height = 6, device = "eps")
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Six.eps', plot = plot.six, width = 4, height = 6, device = "eps")

# To get recombination statistics, select the Day 15 dataset, then exclude the large subclones. Then, calculate the fraction (number) of yPET subclones as a fraction of dTomato subclones. Do this for each colony and then plot the results as a jitter dot plot and place a h-line at the 0.33 mark to denote the expected fraction for subclones

plot.seven <- data.one %>%
             # filter(Condition ==  "DAY15") %>%
             filter(Subclone.Size.Cells %in% (1:50)) %>%
             filter(Colony.ID != "032") %>%
             ungroup() %>%
             group_by(Colony.ID, N, Condition) %>%
             summarize(count.ypet = sum(Subclone.Color == 'yPET'), count.tomato = sum(Subclone.Color == 'dTomato'), total = n()) %>%
             mutate(tDtomato = count.tomato / total, yPET = count.ypet / total) %>%
             ungroup() %>%
             select(N, tDtomato, yPET) %>%
             gather('tDtomato', 'yPET', key = 'color', value = 'percentage') %>%
             ggplot(aes(x = color, y = percentage)) +
             geom_jitter(shape = 21, fill = 'gray', size = 2) +
             theme_publication() +
             labs(x = "Subclone", y = "Recombination Frequency") +
             geom_hline(aes(yintercept = 0.33, color = 'red'), linetype = 'dashed', lwd = 1.2) +
             theme(legend.position = 'none') +
             stat_summary(fun.data=mean_sdl, geom="pointrange", color="red")

ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Seven.eps', plot = plot.seven, width = 4, height = 6, device = "eps")

# Plot distance by clone size metric for both colors on the same graph, try to choose a power law fit to describe the data. State the goodness of the fit for that.
# Normalize the maximum distance to within each colony

plot.eight <- data.one %>%
              group_by(Condition) %>%
              mutate(D.Norm = D / max(D), Size.Norm = Norm.Subclone.Size / max(Norm.Subclone.Size)) %>%
              filter(D.Norm == 0) %>%
              ggplot(aes(x = Condition, y = Norm.Subclone.Size, fill = Subclone.Color)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
              theme_publication() +
              theme(legend.position = 'none') +
              scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
              stat_compare_means(comparisons = list(c("DAY03", "DAY10"), c("DAY10", "DAY15"), c("DAY03", "DAY15")), size = 4, method = 't.test', symnum.args = symnum.args) +
              scale_fill_manual(values=c('#FF6347','#E69F00')) +
              labs(x = "Labeling Time", y = "Subclone Size (Normalized)")


plot.nine <- data.one %>%
              group_by(Condition) %>%
              mutate(D.Norm = D / max(D), Size.Norm = Norm.Subclone.Size / max(Norm.Subclone.Size)) %>%
              filter(D.Norm > 0.85) %>%
              ggplot(aes(x = Condition, y = Norm.Subclone.Size, fill = Subclone.Color)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
              theme_publication() +
              theme(legend.position = 'none') +
              scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
              stat_compare_means(comparisons = list(c("DAY03", "DAY10"), c("DAY10", "DAY15"), c("DAY03", "DAY15")), size = 4, method = 't.test', symnum.args = symnum.args) +
              scale_fill_manual(values=c('#FF6347','#E69F00')) +
              labs(x = "Labeling Time", y = "Subclone Size (Normalized)")

ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Eight.eps', plot = plot.eight, width = 4, height = 6, device = "eps")
ggsave('./analysis/size-distribution-labeling-days/reports/figures/Plot-Nine.eps', plot = plot.nine, width = 4, height = 6, device = "eps")


# The above graph cannot be accomplished on this set of data since the colonies all touch the boundary. What needs to be done is to access those sets of colony data where the labeling is taking place early, but as well there should be a growth frontier that also doesn't touch the boundary. Clone size by distance should be calculated there.
