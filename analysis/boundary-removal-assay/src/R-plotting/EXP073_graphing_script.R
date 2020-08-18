### Start of Script

library('tidyverse')
library('ggpubr')
library('scales')

setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/boundary-removal-assay/')
source('./src/R-tools/graphing_theme.R')

data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

# Scientific Notation
scientific <- function(x){
    ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}

# Internal FX
'%!in%' <- function(x,y)!('%in%'(x,y))

# Plotting Tools
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Data
df.one <- read.csv('./reports/Area_Data.csv')
df.two <- read.csv('./reports/Summary_Data.csv')

# Plot
plot.one <- df.one %>%
            filter(Group %!in% c('yPET / Chronic Mad2')) %>%
            ggplot(aes(x = Group, y = area_microns, fill = Group)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            theme(legend.position = 'none') +
            scale_y_continuous(labels = scientific) +
            stat_compare_means(comparisons = list(c("yPET / Acute Mad2", "yPET / tDT")), size = 4, symnum.args = symnum.args, method = 't.test')

plot.two <- df.two %>%
            group_by(ID) %>%
            filter(ID %!in% c('yPET/ Chronic Mad2')) %>%
            ggplot(aes(x = ID, y = n, fill = ID)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            theme(legend.position = 'none') +
            stat_compare_means(comparisons = list(c("yPET/ Acute Mad2", "yPET/ tDT")), size = 4, symnum.args = symnum.args, method = 't.test')


for(i in ls(pattern = "plot")){
  new <- str_replace_all(i, "[\\.]", '-')
  xx <- paste('./reports/figures/', new, '.eps', sep = "")
  ggsave(xx, get(i), device = 'eps')
}
