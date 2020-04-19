# Exploratory Data analysis for Figure 4
# By: Ajay Bhargava
# 14/04/20

library(ggpubr)
library(gridExtra)
library(tidyverse)
source('./src/R-tools/graphing_theme.R')

# Load Data
input <- readRDS('./data/R-outputs/Fig4-Subclone-Size-Distribution.rds')

# Manipulate dataframe to remove other treatments
data <- input %>% filter(Treatment %in% c("BSR10", "BSR10TGFB"))

# Mutate Total yPET / dTomato subclone area as a fraction of total area across conditions
plot.one.data <- data %>%
group_by(Colony.ID, Treatment, Subclone.Color) %>%
summarize(Subclone.Area = sum(Subclone.Size)) %>%
ungroup() %>%
group_by(Colony.ID) %>%
mutate(Total.Subclone.Area = sum(Subclone.Area)) %>%
mutate(PCT.Area = (Subclone.Area/Total.Subclone.Area)*100) %>%
ungroup() %>%
select(c(Colony.ID, Treatment, PCT.Area, Subclone.Color)) %>%
group_by(Colony.ID) %>%
spread(., Subclone.Color, PCT.Area) %>%
mutate(Relative.Area.yPET.dTomato = yPET/dTomato)

# Mutate % of EdU +'ve' subclones as a percentage of total # of subclones
plot.two.data <- data %>%
group_by(Colony.ID, Treatment, Subclone.Color) %>%
summarize(EdU.Total = sum(EdU.per.Subclone)) %>%
ungroup() %>%
group_by(Colony.ID) %>%
mutate(Total.Subclone.EdU = sum(EdU.Total)) %>%
mutate(PCT.EdU = (EdU.Total/Total.Subclone.EdU)*100) %>%
ungroup() %>%
select(c(Colony.ID, Treatment, PCT.EdU, Subclone.Color)) %>%
group_by(Colony.ID) %>%
spread(., Subclone.Color, PCT.EdU) %>%
mutate(Relative.EdU.yPET.dTomato = yPET/dTomato)

# Mutate % of distinct subclones as a percentage of total # of subclones
plot.three.data <- data %>%
group_by(Colony.ID, Treatment, Subclone.Color) %>%
summarize(num = n()) %>%
ungroup() %>%
group_by(Colony.ID) %>%
mutate(total.num = sum(num)) %>%
mutate(PCT.Num = (num/total.num) * 100) %>%
ungroup() %>%
select(c(Colony.ID, Treatment, PCT.Num, Subclone.Color)) %>%
group_by(Colony.ID) %>%
spread(., Subclone.Color, PCT.Num) %>%
mutate(Relative.Num.yPET.dTomato = yPET/dTomato)


# # Compare the means for statistical significance
# compare_means(Relative.Area.yPET.dTomato ~ Treatment, plot.one.data)
# compare_means(Relative.EdU.yPET.dTomato ~ Treatment, plot.two.data)
# compare_means(Relative.Num.yPET.dTomato ~ Treatment, plot.three.data)
#
# # Demonstration of the means for each thing compared
# plot.one.data %>%
# group_by(Treatment) %>%
# summarize(Relative.Area = mean(Relative.Area.yPET.dTomato), n = n())
#
# plot.two.data %>%
# group_by(Treatment) %>%
# summarize(Relative.EdU = mean(Relative.EdU.yPET.dTomato), n = n())
#
plot.three.data %>%
group_by(Treatment) %>%
summarize(Relative.Num = mean(Relative.Num.yPET.dTomato), n = n())
# # End of Live comparisons of data


# Data for ECDF comparing 4 ECDF plots (or 4 distributions)
plot.four.data <- data %>% select(Colony.ID, Subclone.Size, Norm.Subclone.Size, Subclone.Color, Treatment)
plot.four.data[plot.four.data$Treatment == "BSR10" & plot.four.data$Subclone.Color == "dTomato",]$Treatment <- "BSR10-DT"
plot.four.data[plot.four.data$Treatment == "BSR10" & plot.four.data$Subclone.Color == "yPET",]$Treatment <- "BSR10-YP"
plot.four.data[plot.four.data$Treatment == "BSR10TGFB" & plot.four.data$Subclone.Color == "dTomato",]$Treatment <-"BSR10TGFB-DT"
plot.four.data[plot.four.data$Treatment == "BSR10TGFB" & plot.four.data$Subclone.Color == "yPET",]$Treatment <- "BSR10TGFB-YP"
plot.four.data <- plot.four.data %>% ungroup() %>% select(Subclone.Size, Norm.Subclone.Size, Treatment)
# To get an idea of the frequency distribution
# transform(as.data.frame(table(factor(cut(x, breaks=100)))), cumFreq = cumsum(Freq), relative = prop.table(Freq))


# Graphing Params:
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Plotting the comparison between BSR and BSR10TGFB (Ratio of Areas)
plot.one <- ggplot(plot.one.data, aes(x = Treatment, y = Relative.Area.yPET.dTomato)) + geom_boxplot(lwd = 1.2) + theme_publication()
plot.one <- plot.one + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 3, method = 't.test') + theme(axis.title.x = element_blank())
plot.one <- plot.one + labs(x = "Treatment", y = expression(paste("Ratio of Area of Therapy-Resistant /", "Sensitive Subclones", sep = "\n")))

#Plotting the comparison between BSR and BSR10TGFB (Ratio of EdU)
plot.two <- ggplot(plot.two.data, aes(x = Treatment, y = Relative.EdU.yPET.dTomato)) + geom_boxplot(lwd = 1.2) + theme_publication()
plot.two <- plot.two + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 3, method = 't.test') + theme(axis.title.x = element_blank())
plot.two <- plot.two + labs(x = "Treatment", y = expression(paste("Ratio of EdU"^{"+"}, " Therapy-Resistant /", " Sensitive Subclones", sep = "\n")))

#Plot the comparison between BSR and BSR10TGFB (Ratio of Distinct # of Subclones)
plot.three <- ggplot(plot.three.data, aes(x = Treatment, y = Relative.Num.yPET.dTomato)) + geom_boxplot(lwd = 1.2) + theme_publication()
plot.three <- plot.three + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 3, method = 't.test') + theme(axis.title.x = element_blank())
plot.three <- plot.three + labs(x = "Treatment", y = expression(paste("Ratio of # of ", " Therapy-Resistant /", " Sensitive Subclones", sep = "\n")))

#Plot ECDF for BSR-DT CTRL vs TGFB
plot.four <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10-DT", "BSR10TGFB-DT")), aes(Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.four <- plot.four + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.four <- plot.four + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.four <- plot.four + labs(x = "Subclone size", y = "Cumulative Frequency")


#Plot ECDF for BSR-YP CTRL vs TGFB
plot.five <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10-YP", "BSR10TGFB-YP")), aes(Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.five <- plot.five + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.five <- plot.five + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.five <- plot.five + labs(x = "Subclone size", y = "Cumulative Frequency")

#Plot ECDF for BSR-DT CTRL vs TGFB
plot.six <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10-DT", "BSR10TGFB-DT")), aes(Norm.Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.six <- plot.six + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.six <- plot.six + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.six <- plot.six + labs(x = "Normalized Subclone size", y = "Cumulative Frequency")


#Plot ECDF for BSR-YP CTRL vs TGFB
plot.seven <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10-YP", "BSR10TGFB-YP")), aes(Norm.Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.seven <- plot.seven + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.seven <- plot.seven + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.seven <- plot.seven + labs(x = "Normalized Subclone size", y = "Cumulative Frequency")


# For same colonies
plot.eight <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10-YP", "BSR10-DT")), aes(Norm.Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.eight <- plot.eight + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.eight <- plot.eight + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.eight <- plot.eight + labs(x = "Normalized Subclone size", y = "Cumulative Frequency")

plot.nine <- ggplot(data=plot.four.data %>% filter(Treatment %in% c("BSR10TGFB-YP", "BSR10TGFB-DT")), aes(Norm.Subclone.Size, linetype = Treatment)) + stat_ecdf(geom = "step", pad = TRUE)
plot.nine <- plot.nine + theme_publication() + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.nine <- plot.nine + annotation_logticks(sides = "lb") + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
plot.nine <- plot.nine + labs(x = "Normalized Subclone size", y = "Cumulative Frequency")


grid.arrange(plot.eight, plot.nine)

grid.arrange(plot.four, plot.five, plot.six, plot.seven)


ggsave('./reports/figures/Figure-4/Plot-One.eps', plot = plot.one, width = 4, height = 4, device = "eps")
ggsave('./reports/figures/Figure-4/Plot-Two.eps', plot = plot.two, width = 4, height = 6, device = "eps")
ggsave('./reports/figures/Figure-4/Plot-Three.eps', plot = plot.three, width = 4, height = 6, device = "eps")
ggsave('./reports/figures/Figure-4/Plot-Four.eps', plot = plot.four, width = 6, height = 4, device = "eps")
ggsave('./reports/figures/Figure-4/Plot-Five.eps', plot = plot.five, width = 6, height = 4, device = "eps")
