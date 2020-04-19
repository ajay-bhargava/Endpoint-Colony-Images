plot.clone.size.per.colony <- function(df){
  # Function returns a variable list of plot objects that contains the following information

    # 1) Plot of the clone size distribution for each colony ID
    # 2) Inset of the clone size as a function of distance from the boundary
    # 3) Inset of the clone size as a function of # of EdU per clone

  # By: Ajay Bhargava
  # 20/02/20

  source("./src/tools/graphing_theme.R")
  source("./src/tools/remove-null.R")
  library(gridExtra)
  library(tidyverse)

  # Pre-process the data to make the sizes into cells so that plotting the data is easier, remove erroneous columns
  df$Subclone.Size.Cells <- floor(df$Subclone.Size / 250)
  df$EdU.Per.Subclone.Cells <- ifelse(df$EdU.per.Subclone >= df$Subclone.Size.Cells, df$Subclone.Size.Cells, df$EdU.per.Subclone)
  df <- df %>% select(-c(EdU.per.Subclone, Subclone.Size))
  df <- df[df$Subclone.Size.Cells != 0,]
  setwd("./reports/figures/clone-size-distribution/")
  for (qq in as.character(unique(df$Colony.ID))){
    plot1 <- ggplot(df %>% filter(Colony.ID == qq), aes(x = Colony.ID, y = Subclone.Size.Cells, color = Subclone.Color)) + geom_jitter(shape = 21, width = 0.2, color = "black", aes(fill = factor(Subclone.Color)))
    plot1 <- plot1 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
    plot1 <- plot1 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none') + scale_fill_manual(values = c("firebrick2", "gold1"))
    plot1 <- plot1 + labs(x = "Colony ID", y = (paste("Subclone Size", "(Cells)", sep = "\n")))
    plot2 <- ggplot(df %>% filter(Colony.ID == qq), aes(x = EdU.Per.Subclone.Cells, y = Subclone.Size.Cells, color = Subclone.Color)) + geom_point(shape = 21, color = "black", aes(fill = factor(Subclone.Color)))
    plot2 <- plot2 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
    plot2 <- plot2 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none') + scale_fill_manual(values = c("firebrick2", "gold1"))
    plot2 <- plot2 + labs(x = (paste("EdU +'ve Cells /", "Subclone", sep = "\n")), y = (paste("Subclone Size", "(Cells)", sep = "\n")))
    plot3 <- ggplot(df %>% filter(Colony.ID == qq), aes(x = D.Min, y = Subclone.Size.Cells, color = Subclone.Color)) + geom_point(shape = 21, color = "black", aes(fill = factor(Subclone.Color)))
    plot3 <- plot3 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
    plot3 <- plot3 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none') + scale_fill_manual(values = c("firebrick2", "gold1"))
    plot3 <- plot3 + labs(x = (paste("Distance from", "Boundary (px)", sep = "\n")), y = (paste("Subclone Size", "(Cells)", sep = "\n")))
    layout <- rbind(c(1,1,1,2,2),
                    c(1,1,1,2,2),
                    c(1,1,1,3,3),
                    c(1,1,1,3,3))
    title <- df %>% filter(Colony.ID == qq)
    final.plot <- arrangeGrob(plot1, plot2, plot3, layout_matrix = layout, top = paste("File ID:", paste(title$N[1], title$Colony.ID[1], title$Treatment[1], sep = "-"),".tiff", sep = " "))
    ggsave(file = paste(paste(title$N[1], title$Colony.ID[1], title$Treatment[1], sep = "-"),".pdf", sep = ""), final.plot)
    dev.off()
  }

  # Saves for all Treatment Data
  plot4 <- ggplot(df, aes(x = Treatment, y = Subclone.Size.Cells, color = Treatment)) + geom_jitter(shape = 21, width = 0.2, color = "black", aes(fill = factor(Treatment)))
  plot4 <- plot4 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
  plot4 <- plot4 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
  plot4 <- plot4 + labs(x = "Treatment", y = (paste("Subclone Size", "(Cells)", sep = "\n")))
  plot5 <- ggplot(df, aes(x = EdU.Per.Subclone.Cells, y = Subclone.Size.Cells, color = Treatment)) + geom_point(shape = 21, color = "black", aes(fill = factor(Treatment)))
  plot5 <- plot5 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
  plot5 <- plot5 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
  plot5 <- plot5 + labs(x = (paste("EdU +'ve Cells /", "Subclone", sep = "\n")), y = (paste("Subclone Size", "(Cells)", sep = "\n")))
  plot6 <- ggplot(df, aes(x = D.Min, y = Subclone.Size.Cells, color = Treatment)) + geom_point(shape = 21, color = "black", aes(fill = factor(Treatment)))
  plot6 <- plot6 + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(NA, 1000))
  plot6 <- plot6 + theme_Publication() + annotation_logticks(sides = "l") + theme(legend.position = 'none')
  plot6 <- plot6 + labs(x = (paste("Distance from", "Boundary (px)", sep = "\n")), y = (paste("Subclone Size", "(Cells)", sep = "\n")))
  layout <- rbind(c(1,1,1,2,2),
                  c(1,1,1,2,2),
                  c(1,1,1,3,3),
                  c(1,1,1,3,3))
  final.plot.t <- arrangeGrob(plot4, plot5, plot6, layout_matrix = layout, top = "All Treatments")
  ggsave(file = "All-Treatment-Data.pdf", final.plot.t)
  dev.off()
  setwd('~/Documents/EXP087-Subclone-size-distribution-measurement/')
}
