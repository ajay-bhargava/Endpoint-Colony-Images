colony.area.centroid.analysis <- function(folder.path){
  source('./src/R-tools/shoelace-area-algorithm.R')
  source('./src/R-tools/centroid-tools.R')
  library(tidyverse)
  file.list <- list.files(path = folder.path, pattern = '-Colony-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  colony.location <- list.files(path = folder.path, pattern = '-Colony-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  colony <- lapply(colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
    c <- b[-c(1),]
  })
  split.location <- str_split_fixed(str_extract(colony.location, '\\d{1,3}-[:alnum:]{3,15}-N\\d'), '-', 3)
  colony.location.index <- tibble(N = split.location[,3], Colony.ID = split.location[,1], Treatment = split.location[,2])
  colony.location.table <- split(colony.location.index, sort(as.numeric(rownames(colony.location.index))))
  colony.area <- map2(colony, colony.location.table, function(colony, id){
    data <- data.frame(N = id$N, Colony.ID = id$Colony.ID, Treatment = id$Treatment, Colony.Size = area(colony), Centroid.X = centroid.x(colony), Centroid.Y = centroid.y(colony))
  })
  return(do.call(rbind, colony.area))
}
