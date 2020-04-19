colony.area.analysis <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 31/01/20
  # Function input is a list of folders, lapplys through the list then at the end concatenates the nested dataframe into a single table
  # Function output is a table containing the N, ID, Treatment, and area of each colony
  # Functions are relative to the home directory of this experiment.
  # Usage:
  # data.path <- c('~/Documents/EXP087-Subclone-size-distribution-measurement/data/processed')
  # df <- colony.area.analysis(data.path)
  # Works


  # Returns:
  # # A tibble: 1 x 4
  #   N     Colony.ID Treatment Colony.Size
  #   <chr> <chr>     <chr>           <dbl>
  # 1 N3    01        CTRL         3185132.
  source('./src/R-tools/shoelace-area-algorithm.R')
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
    data <- data.frame(N = id$N, Colony.ID = id$Colony.ID, Treatment = id$Treatment, Colony.Size = area(colony))
  })
  return(do.call(rbind, colony.area))
}
