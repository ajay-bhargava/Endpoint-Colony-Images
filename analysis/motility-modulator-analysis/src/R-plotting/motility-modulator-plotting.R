# ###
#
# Motility modulator analysis
# By: Ajay Bhargava
# 07/13/2020
#
# ##

# Pictures that represent the data

# # A tibble: 18 x 5
# # Groups:   Treatment [3]
#    Treatment Colony.ID         Var       SD Size
#    <chr>     <chr>           <dbl>    <dbl> <fct>
#  1 CTRL      009       0.00211     0.0459   XL
#  2 CTRL      012       0.000898    0.0300   XL
#  3 CTRL      016       0.000569    0.0239   XL
#  4 CTRL      015       0.000338    0.0184   XL
#  5 SMIFH2    061       0.000323    0.0180   L   $
#  6 CTRL      042       0.000194    0.0139   L
#  7 CTRL      013       0.000143    0.0119   XL
#  8 CTRL      003       0.0000909   0.00953  XL
#  9 CTRL      048       0.0000908   0.00953  XL  $
# 10 CTRL      005       0.0000907   0.00952  XL
# 11 CTRL      010       0.0000645   0.00803  XL
# 12 CTRL      006       0.0000603   0.00777  XL
# 13 TGFBE     083       0.0000340   0.00583  L
# 14 TGFBE     118       0.00000688  0.00262  L
# 15 TGFBE     049       0.00000291  0.00171  XL  $
# 16 TGFBE     058       0.00000291  0.00171  XL
# 17 TGFBE     131       0.000000980 0.000990 XL
# 18 TGFBE     056       0.000000916 0.000957 L

# Scaling
scale <- 0.4023

data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

# Internal FX
'%!in%' <- function(x,y)!('%in%'(x,y))

# Plotting Tools
symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

# Load Libraries
setwd('/Users/bhargaa/Documents/Experiments/Endpoint-Colony-Images/analysis/motility-modulator-analysis/')
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

# #######
#
# Process RDS.ONE
# Process the subclone size distribution such that a comparison between "CTRL", "TGFBE", "TGFBL", and "SMIFH2" can be made.
#
# #######

# Plot One of Subclone Size Deviation across treatments
xlabs.one <- paste(RDS.ONE %>%
                    select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
                    filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
                    mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                    ungroup() %>%
                    mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
                    ungroup() %>%
                    mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    group_by(Treatment, Colony.ID) %>%
                    summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size), Size = mean(fold.tumor.size)) %>%
                    replace_na(list(Var = 0, SD = 0)) %>%
                    ungroup() %>%
                    mutate(Size=cut(Size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    filter(Size %in% c("L", "XL")) %>%
                    group_by(Treatment) %>%
                    summarize(N = n_distinct(Colony.ID)) %>%
                    .$Treatment,
                    "\n(N = " ,
                    RDS.ONE %>%
                    select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
                    filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
                    mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
                    ungroup() %>%
                    mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
                    ungroup() %>%
                    mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    group_by(Treatment, Colony.ID) %>%
                    summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size), Size = mean(fold.tumor.size)) %>%
                    replace_na(list(Var = 0, SD = 0)) %>%
                    ungroup() %>%
                    mutate(Size=cut(Size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    filter(Size %in% c("L", "XL")) %>%
                    group_by(Treatment) %>%
                    summarize(N = n_distinct(Colony.ID)) %>%
                  .$N, ")", sep = "")



plot.one <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            ungroup() %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            group_by(Treatment, Colony.ID) %>%
            summarize(Var = var(Norm.Subclone.Size), SD = sd(Norm.Subclone.Size), Size = mean(fold.tumor.size)) %>%
            replace_na(list(Var = 0, SD = 0)) %>%
            ungroup() %>%
            mutate(Size=cut(Size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            filter(Size %in% c("L", "XL")) %>%
            ggplot(aes(x = Treatment, y = SD, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            # stat_summary(geom = "errorbar",
            #             width = 0.1,
            #             fun.ymax = function(x) mean(x) + sd(x) / sqrt(length(x)),
            #             fun.ymin = function(x) mean(x) - sd(x) / sqrt(length(x))) +
            stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args) +
            labs(x = "Condition", y = "Subclone Size Distribution \n Standard Deviation") +
            scale_x_discrete(labels=xlabs.one) +
            theme(legend.position = 'none')

# Plot two of ECDF by treatment
plot.two <- RDS.ONE %>%
            select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
            filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
            mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size) %>%
            ungroup() %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            ungroup() %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            filter(Size %in% c("L", "XL")) %>%
            ggplot(aes(Norm.Subclone.Size, color = Treatment)) +
            stat_ecdf(size = 1, geom = "step", pad = FALSE) +
            theme_publication() +
            scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Subclone Size (Normalized)", y = "ECDF") +
            annotation_logticks(sides = "b")

# #######
#
# Process RDS.TWO -> data.three
# Process the data such that a comparison between "CTRL", "TGFBE", "TGFBL", and "SMIFH2" can be made.
#
# #######

data.three <- RDS.TWO %>%
              select("N", "Colony.ID", "Treatment", "Colony.Size", "Centroid.X", "Centroid.Y", "D.Free", "D.Well", "X", "Y") %>%
              filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
              mutate(D.Centroid.to.EdU = sqrt((X - Centroid.X)^2 + (Y - Centroid.Y)^2)) %>%
              mutate(EdU.Free = (D.Free) / (D.Free +  D.Centroid.to.EdU)) %>%
              mutate(EdU.Well = (D.Well) / (D.Well +  D.Centroid.to.EdU)) %>%
              mutate(EdU.Class = if_else(D.Well < 100 & D.Free < 100, "Confound", if_else(D.Well > 100 & D.Free < 100, "Free", if_else(D.Well < 100 & D.Free > 100, "Boundary", if_else(D.Well > 100 & D.Free > 100, "Inside", "Not-Sure"))))) %>%
              mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
              mutate(Colony.Size = Colony.Size / scale) %>%
              ungroup() %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              select(N, Treatment, Colony.ID, Colony.Size, EdU.Free, EdU.Well, EdU.Class, Size, fold.tumor.size) %>%
              as_tibble()

# # Colonies that don't touch a boundary
# data.four <- data.three %>% filter(is.na(EdU.Class)) %>% select(-c(EdU.Well, EdU.Class))

# Plot three, four, five (Distribution of EDU points by Treatment)
xlabs.three <- paste(data.three %>%
                     filter(EdU.Class %!in% c("Boundary", "Confound", "Not-Sure")) %>%
                     filter(Size %in% c("L", "XL")) %>%
                     group_by(Treatment) %>%
                     summarize(N = n_distinct(Colony.ID)) %>%
                     .$Treatment,
                     "\n(N = " ,
                     data.three %>%
                     filter(EdU.Class %!in% c("Boundary", "Confound", "Not-Sure")) %>%
                     filter(Size %in% c("L", "XL")) %>%
                     group_by(Treatment) %>%
                     summarize(N = n_distinct(Colony.ID)) %>%
                     .$N, ")", sep = "")

plot.three <- data.three %>%
              filter(EdU.Class %!in% c("Boundary", "Confound", "Not-Sure")) %>%
              filter(Size %in% c("L", "XL")) %>%
              group_by(Colony.ID, Treatment) %>%
              summarize(mean.free = mean(EdU.Free), sd.free = sd(EdU.Free)) %>%
              ggplot(aes(y = sd.free, x = Treatment, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              labs(x = "Treatment", y = "Standard Deviation of EdU Position \n (normalized to colony size)") +
              stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args, method = 't.test') +
              scale_x_discrete(labels=xlabs.three) +
              theme(legend.position = 'none')

# plot.four <- data.three %>%
#               filter(EdU.Class %!in% c("Boundary", "Confound", "Not-Sure")) %>%
#               filter(Size %in% c("L", "XL")) %>%
#               group_by(Colony.ID, Treatment) %>%
#               summarize(mean.free = mean(EdU.Free), sd.free = sd(EdU.Free)) %>%
#               ggplot(aes(y = mean.free, x = Treatment, fill = Treatment)) +
#               geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
#               stat_summary(fun.data=data_summary, color="black") +
#               theme_publication() +
#               labs(x = "Treatment", y = "Mean position of EdU+'ve cells in colony \n (normalized to colony size)") +
#               stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args, method = 't.test') +
#               scale_x_discrete(labels=xlabs.three) +
#               ylim(0,1) +
#               theme(legend.position = 'none')

plot.five <- data.three %>%
             filter(EdU.Class %!in% c("Boundary", "Confound", "Not-Sure")) %>%
             filter(Size %in% c("L", "XL")) %>%
             group_by(Colony.ID, Treatment) %>%
             summarize(mean.size = mean(Colony.Size)) %>%
             ggplot(aes(y = mean.size, x = Treatment, fill = Treatment)) +
             geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
             stat_summary(fun.data=data_summary, color="black") +
             theme_publication() +
             labs(x = "Treatment", y = "Colony Size (microns)") +
             stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args, method = 't.test') +
             scale_x_discrete(labels=xlabs.three) +
             theme(legend.position = 'none')

# #######
#
# Process RDS.THREE -> data.four
# Process the data such that a comparison between "CTRL", "TGFBE", "TGFBL", and "SMIFH2" can be made.
#
# #######


xlabs.six <- paste(RDS.THREE %>%
                    mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
                    ungroup() %>%
                    mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
                    filter(Size %in% c("L", "XL")) %>%
                    select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
                    group_by(N, Colony.ID, Treatment, Subclone.ID, P.Free) %>%
                    mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
                    summarize(P = sum(L)) %>%
                    ungroup() %>%
                    mutate(P.Fraction = P / P.Free) %>%
                    group_by(N, Colony.ID, Treatment) %>%
                    summarize(P.Fraction.Colony = mean(P.Fraction)) %>%
                    group_by(Treatment) %>%
                    summarize(N = n_distinct(Colony.ID)) %>%
                    .$Treatment,
                    "\n(N = " ,
                    RDS.THREE %>%
                    mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
                    ungroup() %>%
                    mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
                    filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
                    filter(Size %in% c("L", "XL")) %>%
                    select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
                    group_by(N, Colony.ID, Treatment, Subclone.ID, P.Free) %>%
                    mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
                    summarize(P = sum(L)) %>%
                    ungroup() %>%
                    mutate(P.Fraction = P / P.Free) %>%
                    group_by(N, Colony.ID, Treatment) %>%
                    summarize(P.Fraction.Colony = mean(P.Fraction)) %>%
                    group_by(Treatment) %>%
                    summarize(N = n_distinct(Colony.ID)) %>%
                    .$N, ")", sep = "")

plot.six <- RDS.THREE %>%
            mutate(fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
            ungroup() %>%
            mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
            filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
            filter(Size %in% c("L", "XL")) %>%
            select(N, Colony.ID, Treatment, Subclone.ID, Subclone.Color, X, Y, P.Free, Size) %>%
            group_by(N, Colony.ID, Treatment, Subclone.ID, P.Free) %>%
            mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
            summarize(P = sum(L)) %>%
            ungroup() %>%
            mutate(P.Fraction = P / P.Free) %>%
            group_by(N, Colony.ID, Treatment) %>%
            summarize(P.Fraction.Colony = mean(P.Fraction)) %>%
            ggplot(aes(x = Treatment, y = P.Fraction.Colony, fill = Treatment)) +
            geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
            stat_summary(fun.data=data_summary, color="black") +
            theme_publication() +
            scale_x_discrete(labels = xlabs.six) +
            stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args) +
            labs(x = "Treatment", y = "Subclone boundary dominance") +
            theme(legend.position = 'none')

# To inform readers that the total normalized subclone area across treatments is the same.
plot.seven <- RDS.ONE %>%
              select("N", "Colony.ID", "Treatment", "Colony.Size", "Subclone.ID", "Subclone.Color", "Subclone.Size", "Colony.Size") %>%
              filter(Treatment %in% c("CTRL", "TGFBE", "SMIFH2")) %>%
              mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size, fold.tumor.size = Colony.Size / min(Colony.Size)) %>%
              mutate(Size=cut(fold.tumor.size, breaks=c(-Inf, 1, 5, 10, Inf), labels=c("S", "M", "L", "XL"))) %>%
              group_by(Colony.ID, Treatment, Size) %>%
              summarize(total.subclone.size = sum(Norm.Subclone.Size)) %>%
              ungroup() %>%
              group_by(Treatment, Size) %>%
              filter(Size %in% c("L", "XL")) %>%
              ggplot(aes(x = Treatment, y = total.subclone.size, fill = Treatment)) +
              geom_jitter(shape = 21, size = 2, position=position_jitter(0.1)) +
              stat_summary(fun.data=data_summary, color="black") +
              theme_publication() +
              scale_x_discrete(labels = xlabs.one) +
              stat_compare_means(comparisons = list(c("CTRL", "SMIFH2"), c("CTRL", "TGFBE")), size = 4, symnum.args = symnum.args) +
              labs(x = "Treatment", y = "Total Subclone Area \n (Normalized to Colony Size)") +
              theme(legend.position = 'none')


layout <- rbind(c(1,1,2,2,3,3),
                c(1,1,2,2,3,3),
                c(4,4,5,5,6,6),
                c(4,4,5,5,6,6))
final.plot <- arrangeGrob(plot.one, plot.two, plot.three, plot.seven, plot.five, plot.six, layout_matrix = layout)

ggsave('./reports/figures/Motility-Clone-Size-Distribution-Analysis.eps', final.plot, width = 11, height = 11, device=  "eps")
ggsave('./reports/figures-eps/plot-one.eps', plot.one, width = 5, height = 5, device=  "eps")
ggsave('./reports/figures-eps/plot-two.eps', plot.two, width = 5, height = 5, device=  "eps")
ggsave('./reports/figures-eps/plot-three.eps', plot.three, width = 5, height = 5, device=  "eps")
ggsave('./reports/figures-eps/plot-seven.eps', plot.seven, width = 5, height = 5, device=  "eps")
ggsave('./reports/figures-eps/plot-five.eps', plot.five, width = 5, height = 5, device=  "eps")
ggsave('./reports/figures-eps/plot-six.eps', plot.six, width = 5, height = 5, device=  "eps")
