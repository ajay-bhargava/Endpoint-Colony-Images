###
#
# Therapy resistant subclones analysis
# By: Ajay Bhargava
# 08/01/2020
#
###

# # A tibble: 35 x 3
# # Groups:   Colony.ID [35]
#    Colony.ID Treatment Size
#    <chr>     <chr>     <fct>
#  1 003       CTRL      M
#  2 004       CTRL      M
#  3 005       CTRL      M
#  4 006       CTRL      M
#  5 007       CTRL      M
#  6 008       CTRL      L
#  7 013       CTRL      M
#  8 014       CTRL      L
#  9 015       CTRL      L
# 10 016       CTRL      M
# 11 017       CTRL      M
# 12 019       CTRL      M
# 13 036       CTRL      L
# 14 037       CTRL      M
# 15 038       CTRL      M
# 16 039       CTRL      L
# 17 040       CTRL      M
# 18 041       CTRL      L
# 19 042       CTRL      M
# 20 043       CTRL      L
# 21 044       CTRL      M
# 22 046       CTRL      L
# 23 048       CTRL      L
# 24 127       CTRL      L
# 25 129       CTRL      L
# 26 130       CTRL      L
# 27 137       BSR01     L
# 28 138       BSR01     M
# 29 139       BSR01     M  *
# 30 140       BSR01     M
# 31 142       BSR10     M
# 32 143       BSR10     L
# 33 146       BSR10     M
# 34 147       BSR10     M
# 35 174       CTRL      L

# That Don't Touch
# # A tibble: 13 x 3
# # Groups:   Treatment [4]
#    Treatment Colony.ID Size
#    <chr>     <chr>     <fct>
#  1 BSR01     139       M
#  2 BSR10     146       L
#  3 BSR10TGFB 169       S
#  4 BSR10TGFB 170       M
#  5 BSR10TGFB 171       M
#  6 BSR10TGFB 172       M
#  7 BSR10TGFB 173       M
#  8 CTRL      004       M
#  9 CTRL      017       M
# 10 CTRL      018       M
# 11 CTRL      041       L
# 12 CTRL      129       L
# 13 CTRL      130       L


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


# Validation 0) Does yPET-BSR work in the colony context? If I were to compare the subclonal fraction of yPET and dTomato subclones compared to control, do they follow the above figure?
# Plot.One <- for a subset of colonies between 1-10 times the size of colonies imaged (and this is the grouping used for all analysis), are the colonies significantly different in size? No.
plot.one.a <- RDS.ONE %>%
            group_by(Colony.ID, Treatment) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            summarize(mean.size = mean(Colony.Size)) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Size %in% c("M", "L")) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(y = mean.size, x = Treatment, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            labs(x = "Treatment", y = "Colony Size (microns)") +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            scale_y_continuous(labels = scientific) +
            theme(legend.position = 'none')

# For the same colonies above, what's the clone size distribution and spread of the sizes of the yPET subclones?
# The expectation is that the subclone sizes for yPET-BSR and dTomato will have lower standard deviation, and sharper ECDF.
# Plot Two <- Size Distribution Standard Deviation and ECDF
plot.one.b <- RDS.ONE %>%
            group_by(Colony.ID, Treatment) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Subclone.Color, Treatment) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size), Class = unique(Size), Size = mean(fold.tumor.size)) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Class %in% c("M", "L")) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            filter(Subclone.Color %in% c("yPET")) %>%
            ggplot(aes(x = Treatment, y = SD)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone Size Distribution \n Standard Deviation") +
            theme(legend.position = 'none')

plot.one.c <- RDS.ONE %>%
            group_by(Colony.ID, Treatment) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Subclone.Color, Treatment) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size), Class = unique(Size), Size = mean(fold.tumor.size)) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Class %in% c("M", "L")) %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            filter(Subclone.Color %in% c("dTomato")) %>%
            ggplot(aes(x = Treatment, y = SD)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone Size Distribution \n Standard Deviation") +
            theme(legend.position = 'none')

# plot.two.c and two.d ECDF
plot.one.d <- RDS.ONE %>%
                group_by(Colony.ID, Treatment) %>%
                filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
                ungroup() %>%
                mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                group_by(Colony.ID, Subclone.Color, Treatment, Colony.Size, Size) %>%
                filter(Colony.ID %!in% c("170")) %>%
                filter(Size %in% c("M", "L")) %>%
                ungroup() %>%
                mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
                filter(Subclone.Color %in% c("yPET")) %>%
                ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
                stat_ecdf(size = 1, geom = "step", pad = FALSE) +
                theme_publication() +
                scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
                labs(x = "Subclone Size (Normalized)", y = "ECDF") +
                annotation_logticks(sides = "b")

plot.one.e <- RDS.ONE %>%
                group_by(Colony.ID, Treatment) %>%
                filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
                ungroup() %>%
                mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                group_by(Colony.ID, Subclone.Color, Treatment, Colony.Size, Size) %>%
                filter(Colony.ID %!in% c("170")) %>%
                filter(Size %in% c("M", "L")) %>%
                ungroup() %>%
                mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
                filter(Subclone.Color %in% c("dTomato")) %>%
                ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
                stat_ecdf(size = 1, geom = "step", pad = FALSE) +
                theme_publication() +
                scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
                labs(x = "Subclone Size (Normalized)", y = "ECDF") +
                annotation_logticks(sides = "b")

# Plot.Three <- Subclone Fraction of Total Colony Size (Sum of Subclone Sizes / Total Colony Size) shows the expected - yPET fraction significantly and dose dependently increases,
# dTomato isn't behaving as well as it should but doesn't increase significantly.

plot.two.a <- RDS.ONE %>%
                group_by(Colony.ID, Treatment) %>%
                filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
                ungroup() %>%
                mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                group_by(Colony.ID, Subclone.Color, Treatment, Colony.Size, Size) %>%
                summarize(Subclone.Size = sum(Subclone.Size)) %>%
                summarize(Subclone.Proportion = Subclone.Size / Colony.Size, Class = unique(Size)) %>%
                filter(Colony.ID %!in% c("170")) %>%
                filter(Class %in% c("M", "L")) %>%
                ungroup() %>%
                mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
                filter(Subclone.Color %in% c("yPET")) %>%
                ggplot(aes(x = Treatment, y = Subclone.Proportion)) +
                geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
                stat_summary(fun.data=data_summary, color="black") +
                theme_publication() +
                stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
                labs(x = "Treatment", y = "Subclone \n Proportion of Colony Size") +
                theme(legend.position = 'none')

plot.two.b <- RDS.ONE %>%
                group_by(Colony.ID, Treatment) %>%
                filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
                ungroup() %>%
                mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                group_by(Colony.ID, Subclone.Color, Treatment, Colony.Size, Size) %>%
                summarize(Subclone.Size = sum(Subclone.Size)) %>%
                summarize(Subclone.Proportion = Subclone.Size / Colony.Size, Class = unique(Size)) %>%
                filter(Colony.ID %!in% c("170")) %>%
                filter(Class %in% c("M", "L")) %>%
                ungroup() %>%
                mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
                filter(Subclone.Color %in% c("dTomato")) %>%
                ggplot(aes(x = Treatment, y = Subclone.Proportion)) +
                geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
                stat_summary(fun.data=data_summary, color="black") +
                theme_publication() +
                stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
                labs(x = "Treatment", y = "Subclone \n Proportion of Colony Size") +
                theme(legend.position = 'none')

plot.two.c <- RDS.ONE %>%
                group_by(Colony.ID, Treatment) %>%
                filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
                ungroup() %>%
                mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                group_by(Colony.ID, Subclone.Color, Treatment, Colony.Size, Size) %>%
                summarize(Subclone.Size = sum(Subclone.Size)) %>%
                summarize(Subclone.Proportion = Subclone.Size / Colony.Size, Class = unique(Size)) %>%
                filter(Colony.ID %!in% c("170")) %>%
                filter(Class %in% c("M", "L")) %>%
                ungroup() %>%
                mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
                group_by(Treatment, Colony.ID) %>%
                summarize(Total.Proportion = sum(Subclone.Proportion)) %>%
                ggplot(aes(x = Treatment, y = Total.Proportion, fill = Treatment)) +
                geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
                stat_summary(fun.data=data_summary, color="black") +
                theme_publication() +
                stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
                labs(x = "Treatment", y = "Subclone \n Proportion of Colony Size") +
                theme(legend.position = 'none')

# Confident in my ability to design a system capable of having subclones selectively respond to BSR in a dose dependent manner,
# I next asked what the spatial boundary occupation is for yPET and dTomato subclones c.f. to CTRL and BSR01 and BSR10
plot.three.a <- RDS.THREE %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Size %in% c("M", "L")) %>%
            ungroup() %>%
            select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
            group_by(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, P.Free) %>%
            filter(Subclone.Color %in% c('yPET')) %>%
            mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
            summarize(P = sum(L)) %>%
            ungroup() %>%
            group_by(Colony.ID, Treatment) %>%
            summarize(Sum.P = sum(P), P.Free = mean(P.Free)) %>%
            mutate(P.Fraction = Sum.P / P.Free) %>%
            ungroup() %>%
            filter(Colony.ID %!in% c("129", "130")) %>%
            mutate(P.Fraction = if_else(P.Fraction > 1, 1, P.Fraction)) %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = P.Fraction, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone boundary dominance \n for yPET-BSR Subclones") +
            theme(legend.position = 'none')

plot.three.b <- RDS.THREE %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Size %in% c("M", "L")) %>%
            ungroup() %>%
            select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
            group_by(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, P.Free) %>%
            filter(Subclone.Color %in% c('dTomato')) %>%
            mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
            summarize(P = sum(L)) %>%
            ungroup() %>%
            group_by(Colony.ID, Treatment) %>%
            summarize(Sum.P = sum(P), P.Free = mean(P.Free)) %>%
            mutate(P.Fraction = Sum.P / P.Free) %>%
            ungroup() %>%
            filter(Colony.ID %!in% c("129", "130")) %>%
            mutate(P.Fraction = if_else(P.Fraction > 1, 1, P.Fraction)) %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = P.Fraction, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Subclone boundary dominance \n for yPET-BSR Subclones") +
            theme(legend.position = 'none')

# Then I asked where therapy resistant subclones are proliferating.
# To do this, first, the mode of EdU is going to be determined for each colony. Then, a count will be performed for the number of subclones that sit either ahead or behind this mode.
# A sum of the EdU points across all subclones ahead of this demarcation point and behind will be performed. This will be compared across colonies in conditions.

plot.four.a <- RDS.ONE %>%
             inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
             filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
             mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
             mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
             mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
             group_by(Colony.ID, Treatment, Size) %>%
             filter(Colony.ID %!in% c("170")) %>%
             filter(Size %in% c("M", "L")) %>%
             ungroup() %>%
             group_by(Colony.ID, Treatment, Position, Subclone.Color) %>%
             summarize(sum.edu.subclone = sum(EdU.per.Subclone)) %>%
             filter(Subclone.Color %in% c("yPET")) %>%
             ungroup() %>%
             spread(., Position, sum.edu.subclone) %>%
             drop_na() %>%
             mutate(Ratio = Interior.EdU / Exterior.EdU) %>%
             mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
             ggplot(aes(x = Treatment, y = Ratio)) +
             geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
             stat_summary(fun.data=data_summary, color="black") +
             theme_publication() +
             stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
             labs(x = "Treatment", y = "Ratio of EdU +'ve yPET subclones \n in colony interior vs exterior") +
             theme(legend.position = 'none')

plot.four.b <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            filter(Size %in% c("M", "L")) %>%
            ungroup() %>%
            group_by(Colony.ID, Treatment, Position, Subclone.Color) %>%
            summarize(sum.edu.subclone = sum(EdU.per.Subclone)) %>%
            filter(Subclone.Color %in% c("dTomato")) %>%
            ungroup() %>%
            spread(., Position, sum.edu.subclone) %>%
            drop_na() %>%
            mutate(Ratio = Interior.EdU / Exterior.EdU) %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = Ratio)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Ratio of EdU +'ve dTomato subclones \n in colony interior vs exterior") +
            theme(legend.position = 'none')

# Measuring the diversity index of the colony, looking at the total number of subclones by each color.
plot.ten.a <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
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
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = n)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')


plot.ten.b <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
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
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = n)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')

plot.ten.c <- RDS.ONE %>%
            inner_join(., RDS.TWO %>% group_by(Colony.ID) %>% summarize(edu.ring = mean(D.Free))) %>%
            filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
            mutate(Position = if_else(D.Colony < edu.ring, "Exterior.EdU", "Interior.EdU")) %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Colony.ID, Treatment, Size) %>%
            filter(Colony.ID %!in% c("170")) %>%
            ungroup() %>%
            subset(EdU.per.Subclone != 0) %>%
            group_by(N, Colony.ID, Treatment) %>%
            tally() %>%
            ungroup() %>%
            mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
            ggplot(aes(x = Treatment, y = n, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
            labs(x = "Treatment", y = "Total number of Distinct \n Proliferating Subclones") +
            theme(legend.position = 'none')

# Next, I examined the number and size of annhilated therapy resistant (yPET) subclones.
# An annhilated subclone is designated as one whose border is not shared with the growth perimeter

plot.eleven.a <- RDS.ONE %>%
              anti_join(., RDS.THREE %>% group_by(Colony.ID, Subclone.Color) %>% count(Subclone.ID), by = c("Colony.ID", "Subclone.Color", "Subclone.ID")) %>% # excludes subclones that touch the perimeter.
              filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
              ungroup() %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              filter(Size %in% c("M", "L")) %>%
              ungroup() %>%
              group_by(Colony.ID, Treatment, Subclone.Color) %>%
              filter(Subclone.Color %in% c("yPET")) %>%
              summarize(count = n(), sum.size = sum(Norm.Subclone.Size)) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
              ggplot(aes(x = Treatment,  y = count, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              # stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
              labs(x = "Treatment", y = "Number of \n annhilated subclones") +
              ylim(NA, 2000) +
              theme(legend.position = 'none')

plot.eleven.b <- RDS.ONE %>%
              anti_join(., RDS.THREE %>% group_by(Colony.ID, Subclone.Color) %>% count(Subclone.ID), by = c("Colony.ID", "Subclone.Color", "Subclone.ID")) %>% # excludes subclones that touch the perimeter.
              filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
              ungroup() %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              filter(Size %in% c("M", "L")) %>%
              ungroup() %>%
              group_by(Colony.ID, Treatment, Subclone.Color) %>%
              summarize(count = n(), sum.size = sum(Norm.Subclone.Size)) %>%
              filter(Subclone.Color %in% c("dTomato")) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
              ggplot(aes(x = Treatment,  y = count, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              # stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
              labs(x = "Treatment", y = "Number of \n annhilated subclones") +
              ylim(NA, 2000) +
              theme(legend.position = 'none')

plot.eleven.c <- RDS.ONE %>%
              anti_join(., RDS.THREE %>% group_by(Colony.ID, Subclone.Color) %>% count(Subclone.ID), by = c("Colony.ID", "Subclone.Color", "Subclone.ID")) %>% # excludes subclones that touch the perimeter.
              filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
              ungroup() %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              filter(Size %in% c("M", "L")) %>%
              ungroup() %>%
              group_by(Colony.ID, Treatment, Subclone.Color) %>%
              summarize(count = n(), max.size = max(Norm.Subclone.Size)) %>%
              filter(Subclone.Color %in% c("yPET")) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
              ggplot(aes(x = Treatment,  y = max.size, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "goldenrod2") +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              # stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
              labs(x = "Treatment", y = "Maximum \n annhilated subclone size") +
              theme(legend.position = 'none') +
              ylim(NA, 0.1)

plot.eleven.d <- RDS.ONE %>%
              anti_join(., RDS.THREE %>% group_by(Colony.ID, Subclone.Color) %>% count(Subclone.ID), by = c("Colony.ID", "Subclone.Color", "Subclone.ID")) %>% # excludes subclones that touch the perimeter.
              filter(Treatment %in% c("CTRL", "BSR01", "BSR10")) %>%
              ungroup() %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size), Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              filter(Colony.ID %!in% c("170")) %>%
              filter(Size %in% c("M", "L")) %>%
              ungroup() %>%
              group_by(Colony.ID, Treatment, Subclone.Color) %>%
              summarize(count = n(), max.size = max(Norm.Subclone.Size)) %>%
              filter(Subclone.Color %in% c("dTomato")) %>%
              ungroup() %>%
              mutate(Treatment = as.factor(Treatment) %>% fct_relevel(., levels = c("CTRL", "BSR01", "BSR10"))) %>%
              ggplot(aes(x = Treatment,  y = max.size, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1), fill = "orangered2") +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              # stat_compare_means(comparisons = list(c("CTRL", "BSR10"), c("CTRL", "BSR01")), size = 4, symnum.args = symnum.args, method = 't.test') +
              labs(x = "Treatment", y = "Maximum \n annhilated subclone size") +
              theme(legend.position = 'none') +
              ylim(NA, 0.1)



##########
#
# Compile all the graphs
#
##########

# Dose Dependency Validation of Phenotype
dose.dependency.validation <- arrangeGrob(plot.one.a, plot.one.b, plot.one.c, plot.one.d, plot.one.e)
subclone.proportion.dose.dependency <- arrangeGrob(plot.two.a, plot.two.b, plot.two.c)
spatial.boundary.dose.dependency <- arrangeGrob(plot.three.a, plot.three.b)
therapy.edu.dose.dependency <- arrangeGrob(plot.four.a, plot.four.b)
n.subclones.dose.dependency <- arrangeGrob(plot.ten.a, plot.ten.b, plot.ten.c)

ggsave('./reports/compiled-figures/dose-dependency-validation.eps', dose.dependency.validation, device = 'eps')
ggsave('./reports/compiled-figures/subclone-proportion-dose-dependency.eps', subclone.proportion.dose.dependency, device = 'eps')
ggsave('./reports/compiled-figures/spatial-boundary-dose-dependency.eps', spatial.boundary.dose.dependency, device = 'eps')
ggsave('./reports/compiled-figures/therapy-edu-dose-dependency.eps', therapy.edu.dose.dependency, device = 'eps')
ggsave('./reports/compiled-figures/n-subclones-dose-dependency.eps', n.subclones.dose.dependency, device = 'eps')

for(i in ls(pattern = "plot")){
  new <- str_replace_all(i, "[\\.]", '-')
  xx <- paste('./reports/new-figures/', new, '.eps', sep = "")
  ggsave(xx, get(i), device = 'eps')
}
