colony.area.analysis <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 23/05/2020
  # Finished Build.

  source('./analysis/size-distribution-labeling-days/src/R-functions/shoelace-area-algorithm.R')
  library(tidyverse)
  colony.location <- list.files(path = folder.path, pattern = '-Colony-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  colony <- lapply(colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
    c <- b[-c(1:2),]
  })
  split.location <- str_split_fixed(str_extract(colony.location, '\\d{1,3}-[:alnum:]{1,5}-N\\d'), '-', 3)
  colony.location.index <- tibble(Colony.ID = split.location[,1], Condition = split.location[,2], N = split.location[,3])
  colony.location.table <- split(colony.location.index, sort(as.numeric(rownames(colony.location.index))))
  colony.area <- map2(colony, colony.location.table, function(colony, id){
    data <- data.frame(N = id$N, Colony.ID = id$Colony.ID, Condition = id$Condition, Colony.Size = area(colony))
  })
  return(do.call(rbind, colony.area))
}
