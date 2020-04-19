compare.clonal.ratios <- function(df){
  # Function to return graphical data demonstrating two things:

    # 1) Plot of the ratio of subclone sizes (yPET / dTomato) as a function of treatment. Each colony represents one dot on the plot
    # 2) Comparison of ratio of the mean number of EdU+ve dots for mean of yPET labelled subclones against a dTomato labelled subclones

    source("./src/tools/graphing_theme.R")
    source("./src/tools/remove-null.R")
    library(gridExtra)
    library(tidyverse)
    library(ggpubr)
    df <- readRDS('./notebooks/Clonal-Statistics-pOncobow8-BSR-TGFB.rds')
    # Redefine subclone sizes such that a single subclone is a size of one cell. This involves normalizing the size to a reference value of 250 pixels^2 which represents the size of one cell.
    df$Subclone.Size.Cells <- floor(df$Subclone.Size / 250)
    df$EdU.Per.Subclone.Cells <- ifelse(df$EdU.per.Subclone >= df$Subclone.Size.Cells, df$Subclone.Size.Cells, df$EdU.per.Subclone)
    df <- df %>% select(-c(EdU.per.Subclone, Subclone.Size))
    df <- df[df$Subclone.Size.Cells != 0,]
    df <- df %>% group_by(Colony.ID) %>% mutate(Norm.Subclone.Size.Cells = Subclone.Size.Cells / Colony.Size, Norm.EdU.Per.Subclone.Cells = EdU.Per.Subclone.Cells / Colony.Size)
    df.stats <- df %>% group_by(Treatment, Colony.ID, Subclone.Color) %>% summarize(mean.subclone.size = mean(Norm.Subclone.Size.Cells), mean.edu.per.subclone = mean(Norm.EdU.Per.Subclone.Cells))
    df.stats.size <- df.stats %>% select(-c(mean.edu.per.subclone)) %>% group_by(Colony.ID) %>% spread(Subclone.Color, mean.subclone.size) %>% mutate(size.ratio = yPET/dTomato) %>% select(c("Treatment", "Colony.ID", "size.ratio"))
    df.stats.edu <- df.stats %>% select(-c(mean.subclone.size)) %>% group_by(Colony.ID) %>% spread(Subclone.Color, mean.edu.per.subclone) %>% mutate(edu.ratio = yPET/dTomato) %>% select(c("Treatment", "Colony.ID", "edu.ratio"))
    df.stats.complete <- inner_join(df.stats.size, df.stats.edu)
    df.stats.complete$Treatment <- factor(df.stats.complete$Treatment, levels = c("CTRL", "CTRLTGFB", "BSR01", "BSR01TGFB", "BSR05", "BSR05TGFB", "BSR10", "BSR10TGFB"))
    df$Treatment <- factor(df$Treatment, levels = c("CTRL", "CTRLTGFB", "BSR01", "BSR01TGFB", "BSR05", "BSR05TGFB", "BSR10", "BSR10TGFB"))
    df.stats$Treatment <- factor(df.stats$Treatment, levels = c("CTRL", "CTRLTGFB", "BSR01", "BSR01TGFB", "BSR05", "BSR05TGFB", "BSR10", "BSR10TGFB"))

    # Comparisons of interest
    symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))

    # Ratio of yPET to dTomato subclones
    plot1 <- ggplot(df.stats.complete %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB"), aes(x = Treatment, y = size.ratio)) + geom_boxplot(lwd = 1.05) + theme_Publication() + theme(legend.position = 'none')
    plot1 <- plot1 + labs(x = "Treatment", y = paste("Ratio of Size of", "yPET-BSR/dTomato Clones", sep = "\n")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 3, method = 't.test')

    # Ratio of EdU+'ve' yPET to dTomato subclones
    plot2 <- ggplot(df.stats.complete %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB"), aes(x = Treatment, y = edu.ratio)) + geom_boxplot(lwd = 1.05) + theme_Publication() + theme(legend.position = 'none')
    plot2 <- plot2 + labs(x = "Treatment", y = (paste("Ratio of # of EdU'+ve", "yPET-BSR/dTomato Subclones", sep = "\n"))) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), size = 3, method = 't.test')

    # Normalized Subclone size distribution, jittering by color
    plot3a <- ggplot(df %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "yPET"), aes(x = Treatment, y = Norm.Subclone.Size.Cells, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot3a <- plot3a + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot3a <- plot3a + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
    plot3a <- plot3a + labs(x = "Treatment", y = (paste("Distribution of", "Subclone Sizes (Normalized)", sep = "\n"))) + scale_fill_manual(values = c("gold1")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    plot3b <- ggplot(df %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "dTomato"), aes(x = Treatment, y = Norm.Subclone.Size.Cells, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot3b <- plot3b + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot3b <- plot3b + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
    plot3b <- plot3b + labs(x = "Treatment", y = (paste("Distribution of", "Subclone Sizes (Normalized)", sep = "\n"))) + scale_fill_manual(values = c("firebrick2")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    # EdU Distribution by Color, jittering by color
    plot4a <- ggplot(df %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "dTomato"), aes(x = Treatment, y = Norm.EdU.Per.Subclone.Cells, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot4a <- plot4a + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot4a <- plot4a + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
    plot4a <- plot4a + labs(x = "Treatment", y = (paste("Size of", "EdU+'ve subclones (Normalized)", sep = "\n"))) + scale_fill_manual(values = c("firebrick2")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    plot4b <- ggplot(df %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "yPET"), aes(x = Treatment, y = Norm.EdU.Per.Subclone.Cells, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot4b <- plot4b + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot4b <- plot4b + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
    plot4b <- plot4b + labs(x = "Treatment", y = (paste("Size of", "EdU+'ve subclones (Normalized)", sep = "\n"))) + scale_fill_manual(values = c("gold1")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    # Mean Subclone Size Distribution by Color
    plot5a <- ggplot(df.stats %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "dTomato"), aes(x = Treatment, y = mean.subclone.size, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot5a <- plot5a + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot5a <- plot5a + theme_Publication() + theme(legend.position = 'none')
    plot5a <- plot5a + labs(x = "Treatment", y = (paste("Mean Subclone Size", "(Normalized)", sep = "\n"))) + scale_fill_manual(values = c("firebrick2")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    plot5b <- ggplot(df.stats %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "yPET"), aes(x = Treatment, y = mean.subclone.size, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot5b <- plot5b + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot5b <- plot5b + theme_Publication() + theme(legend.position = 'none')
    plot5b <- plot5b + labs(x = "Treatment", y = (paste("Mean Subclone Size", "(Normalized)", sep = "\n"))) + scale_fill_manual(values = c("gold1")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    # Mean EdU Distribution by Color
    plot6a <- ggplot(df.stats %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "dTomato"), aes(x = Treatment, y = mean.edu.per.subclone, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot6a <- plot6a + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot6a <- plot6a + theme_Publication() + theme(legend.position = 'none')
    plot6a <- plot6a + labs(x = "Treatment", y = (paste("Mean EdU / Subclone", "(Normalized)", sep = "\n"))) + scale_fill_manual(values = c("firebrick2")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    plot6b <- ggplot(df.stats %>% filter(Treatment == "BSR10" | Treatment == "BSR10TGFB") %>% filter(Subclone.Color == "yPET"), aes(x = Treatment, y = mean.edu.per.subclone, fill = Subclone.Color)) + geom_boxplot(lwd = 1.05)
    plot6b <- plot6b + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)))
    plot6b <- plot6b + theme_Publication() + theme(legend.position = 'none')
    plot6b <- plot6b + labs(x = "Treatment", y = (paste("Mean EdU / Subclone", "(Normalized)", sep = "\n"))) + scale_fill_manual(values = c("gold1")) + stat_compare_means(comparisons = list(c("BSR10", "BSR10TGFB")), symnum.args = symnum.args)

    grid.arrange(plot1, plot2, nrow =1, ncol = 2)
}
