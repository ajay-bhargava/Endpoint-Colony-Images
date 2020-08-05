###
#
# Therapy resistant subclones analysis
# By: Ajay Bhargava
# 08/01/2020
#
###

# # A tibble: 8 x 2
# # Groups:   Colony.ID [8]
#   Colony.ID Treatment
#   <chr>     <chr>
# 1 142       BSR10
# 2 143       BSR10
# 3 146       BSR10
# 4 147       BSR10
# 5 169       BSR10TGFB
# 6 171       BSR10TGFB
# 7 172       BSR10TGFB
# 8 173       BSR10TGFB

# Scaling
scale <- 0.4023

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

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/therapy-resistant-subclones/')
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(scales)
source('./src/R-tools/graphing_theme.R')
source('./src/R-tools/roll.R')

# Read in each dataset
RDS.ONE <- readRDS('../../shared-assets-local/Distribution-Dataset-14433144.rds')
RDS.TWO <- readRDS('../../shared-assets-local/EdU-Coordinates-Dataset-14433144.rds')
RDS.THREE <- readRDS('../../shared-assets-local/Spatial-Hedgemony-Dataset-14512621.rds')

# # Validation 1) Colony Sizes between treatments are not significantly different
plot.five.a <- RDS.ONE %>%
             group_by(Colony.ID, Treatment) %>%
             filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
             summarize(mean.size = mean(Colony.Size)) %>%
             filter(Colony.ID %!in% c("170")) %>%
             ggplot(aes(y = mean.size, x = Treatment, fill = Treatment)) +
             geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
             stat_summary(fun.data=data_summary, color="black") +
             theme_publication() +
             labs(x = "Treatment", y = "Colony Size (microns)") +
             stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
             scale_y_continuous(labels = scientific) +
             theme(legend.position = 'none')

# # Validation 3) Subclone Size Distribution Standard Deviation
plot.five.b <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            filter(Subclone.Color %in% c('yPET')) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            group_by(Treatment, Colony.ID) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size)) %>%
            replace_na(list(Var = 0, SD = 0)) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(x = Treatment, y = SD, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            # stat_summary(geom = "errorbar",
            #             width = 0.1,
            #             fun.ymax = function(x) mean(x) + sd(x) / sqrt(length(x)),
            #             fun.ymin = function(x) mean(x) - sd(x) / sqrt(length(x))) +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone Size Distribution \n Standard Deviation") +
            theme(legend.position = 'none')

plot.five.c <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            filter(Subclone.Color %in% c('dTomato')) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            group_by(Treatment, Colony.ID) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size)) %>%
            replace_na(list(Var = 0, SD = 0)) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(x = Treatment, y = SD, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            # stat_summary(geom = "errorbar",
            #             width = 0.1,
            #             fun.ymax = function(x) mean(x) + sd(x) / sqrt(length(x)),
            #             fun.ymin = function(x) mean(x) - sd(x) / sqrt(length(x))) +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone Size Distribution \n Standard Deviation") +
            theme(legend.position = 'none')

# Validation 4) ECDF of subclone size distributions
plot.five.d <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Subclone.Color %in% c('dTomato')) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            ungroup() %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            annotation_logticks(sides = "b")

plot.five.e <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Subclone.Color %in% c('yPET')) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            ungroup() %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            annotation_logticks(sides = "b")

# # Validation 2) yPET and dTomato Proportion of whole colony size
plot.six.a <- RDS.ONE %>%
              select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
              filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
              filter(Subclone.Color %in% c("yPET")) %>%
              group_by(Colony.ID, Treatment, Colony.Size) %>%
              summarize(Subclone.Size = sum(Subclone.Size)) %>%
              summarize(Subclone.Proportion = Subclone.Size / Colony.Size) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              ggplot(aes(x = Treatment, y = Subclone.Proportion, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
              stat_summary(fun.data=data_summary, color="black", fill = "goldenrod2") +
              stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
              theme_publication() +
              labs(x = "Treatment", y = "Subclone \n Proportion of Colony Size") +
              theme(legend.position = 'none')

plot.six.b <- RDS.ONE %>%
              select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
              filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
              filter(Subclone.Color %in% c("dTomato")) %>%
              group_by(Colony.ID, Treatment, Colony.Size) %>%
              summarize(Subclone.Size = sum(Subclone.Size)) %>%
              summarize(Subclone.Proportion = Subclone.Size / Colony.Size) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              ggplot(aes(x = Treatment, y = Subclone.Proportion, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
              stat_summary(fun.data=data_summary, color="black") +
              stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
              theme_publication() +
              labs(x = "Treatment", y = "Subclone \n Proportion of Colony Size") +
              theme(legend.position = 'none')

# Hypothesis test 1) Subclone Dominance between yPET subclones in TGFB and CTRL for BSR10
plot.seven.a <- RDS.THREE %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
            group_by(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, P.Free) %>%
            filter(Subclone.Color %in% c('yPET')) %>%
            mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
            ungroup() %>%
            group_by(N, Colony.ID, Treatment, P.Free) %>%
            summarize(P = sum(L)) %>%
            mutate(P.Fraction = P / P.Free) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(x = Treatment, y = P.Fraction, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone boundary dominance \n for yPET-BSR Subclones") +
            theme(legend.position = 'none')

plot.seven.b <- RDS.THREE %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
            group_by(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, P.Free) %>%
            filter(Subclone.Color %in% c('dTomato')) %>%
            mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
            ungroup() %>%
            group_by(N, Colony.ID, Treatment, P.Free) %>%
            summarize(P = sum(L)) %>%
            mutate(P.Fraction = P / P.Free) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(x = Treatment, y = P.Fraction, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone boundary dominance \n for dTomato Subclones") +
            theme(legend.position = 'none')


# Then I asked where therapy resistant subclones are proliferating.
# To do this, first, the mode of EdU is going to be determined for each colony. Then, a count will be performed for the number of subclones that sit either ahead or behind this mode.
# A sum of the EdU points across all subclones ahead of this demarcation point and behind will be performed. This will be compared across colonies in conditions.

plot.eight.a <- RDS.ONE %>%
             inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
             filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
             mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
             mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
             mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
             group_by(Colony.ID, Treatment, Size) %>%
             filter(Colony.ID %!in% c("170")) %>%
             ungroup() %>%
             group_by(Colony.ID, Treatment, Position, Subclone.Color) %>%
             summarize(sum.edu.subclone = sum(EdU.per.Subclone)) %>%
             filter(Subclone.Color %in% c("yPET")) %>%
             ungroup() %>%
             spread(., Position, sum.edu.subclone) %>%
             drop_na() %>%
             mutate(Ratio = Interior.EdU / Exterior.EdU) %>%
             mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
             ggplot(aes(x = Treatment, y = Ratio)) +
             geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
             stat_summary(fun.data=data_summary, color="black") +
             theme_publication() +
             stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
             labs(x = "Treatment", y = "Ratio of EdU +'ve yPET subclones \n in colony interior vs exterior") +
             theme(legend.position = 'none')

plot.eight.b <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            group_by(Colony.ID, Treatment, Position, Subclone.Color) %>%
            summarize(sum.edu.subclone = sum(EdU.per.Subclone)) %>%
            filter(Subclone.Color %in% c("dTomato")) %>%
            ungroup() %>%
            spread(., Position, sum.edu.subclone) %>%
            drop_na() %>%
            mutate(Ratio = Interior.EdU / Exterior.EdU) %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("BSR10", "BSR10TGFB"))) %>%
            ggplot(aes(x = Treatment, y = Ratio)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Ratio of EdU +'ve dTomato subclones \n in colony interior vs exterior") +
            theme(legend.position = 'none')


# Finally, I examined the 'diversity' of the colony. Here I quantified the number of distinct & proliferating therapy resistant subclones for colonies under treatment, keeping all other parameters same.
plot.nine.a <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            subset(EdU.per.Subclone != 0) %>%
            group_by(N, Colony.ID, Treatment, Subclone.Color) %>%
            tally() %>%
            filter(Subclone.Color %in% c("yPET")) %>%
            ggplot(aes(x = Treatment, y = n)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')


plot.nine.b <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            subset(EdU.per.Subclone != 0) %>%
            group_by(N, Colony.ID, Treatment, Subclone.Color) %>%
            tally() %>%
            filter(Subclone.Color %in% c("dTomato")) %>%
            ggplot(aes(x = Treatment, y = n)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')

plot.nine.c <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("BSR10", "BSR10TGFB")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            subset(EdU.per.Subclone != 0) %>%
            group_by(N, Colony.ID, Treatment) %>%
            tally() %>%
            ggplot(aes(x = Treatment, y = n, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')

##########
#
# Compile all the graphs
#
##########
# Comparison of BSR10 TGFB and CTRL
mixing.validation <- arrangeGrob(plot.five.a, plot.five.b, plot.five.c, plot.five.d, plot.five.e)
mixing.proportion.subclone <- arrangeGrob(plot.six.a, plot.six.b)
spatial.boundary.mixing <- arrangeGrob(plot.seven.a, plot.seven.b)
therapy.edu.mixing <- arrangeGrob(plot.eight.a, plot.eight.b)
n.subclones.dose.mixing <- arrangeGrob(plot.nine.a, plot.nine.b, plot.nine.c)


ggsave('./reports/compiled-figures/mixing-validation.eps', mixing.validation, device = 'eps')
ggsave('./reports/compiled-figures/mixing-proportion-subclone.eps', mixing.proportion.subclone, device = 'eps')
ggsave('./reports/compiled-figures/spatial-boundary-mixing.eps', spatial.boundary.mixing, device = 'eps')
ggsave('./reports/compiled-figures/therapy-edu-mixing.eps', therapy.edu.mixing, device = 'eps')
ggsave('./reports/compiled-figures/n-subclones-dose-mixing.eps', n.subclones.dose.mixing, device = 'eps')

for(i in ls(pattern = "plot")){
  new <- str_replace_all(i, "[\\.]", '-')
  xx <- paste('./reports/new-figures/', new, '.eps', sep = "")
  ggsave(xx, get(i), device = 'eps')
}
