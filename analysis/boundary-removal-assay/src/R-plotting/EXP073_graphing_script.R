'''
EXP072 - This is a graphing script to graph the data from EXP072 comparing the mash assay results of number and average area size between experimental outsets.
'''

# Libraries
library(tidyverse)
library(ggpubr)

# Themes

theme_Publication <- function(base_size=14, base_family="Helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(),
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "right",
               legend.direction = "vertical",
               legend.key.size= unit(0.2, "cm"),
               legend.spacing = unit(0, "cm"),
               legend.title = element_text(face="bold"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))

}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

df_area <- read.csv("/Volumes/SSD/EXP073_simple_mixing_of_clones_to_boundary_then_removal/data/processed/Area_Data.csv")

df_obs <- read.csv("/Volumes/SSD/EXP073_simple_mixing_of_clones_to_boundary_then_removal/data/processed/Summary_Data.csv")

plot <- ggboxplot(df_area, x = "Group", y="area_microns", add = "jitter") + theme_Publication() + stat_compare_means(comparisons = list(c("yPET / Acute Mad2", "yPET / Chronic Mad2"), c("yPET / tDT","yPET / Chronic Mad2")), label = "p.signif") + ylab(expression("Size of micrometastasis (" * mu ~ "m)"))

df_x <- df_obs %>% group_by("ID") %>% summarize(n = mean("ID"))

plot_2 <- ggplot(df_x, aes(x = ID, y = n)) + geom_bar(stat = "identity") + theme_Publication() + ylab("# of micrometastasis")
