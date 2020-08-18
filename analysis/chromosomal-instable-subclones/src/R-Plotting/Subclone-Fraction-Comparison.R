library('tidyverse')
library('ggpubr')

setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/chromosomal-instable-subclones/')
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

# Load Data
df <- read.csv('./reports/RFP_YFP_Quant.csv')

# Plot comparison of RFP to YFP ratio for pOncobow2 vs 3 and plot significance.
plot.one <- df %>%
            ggplot(aes(x = Construct, y = Ratio, fill = Construct)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            theme(legend.position = 'none') +
            stat_compare_means(comparisons = list(c("pOncobow2", "pOncobow3")), size = 4, symnum.args = symnum.args, method = 't.test')

for(i in ls(pattern = "plot")){
  new <- str_replace_all(i, "[\\.]", '-')
  xx <- paste('./reports/figures/', new, '.eps', sep = "")
  ggsave(xx, get(i), device = 'eps')
}
