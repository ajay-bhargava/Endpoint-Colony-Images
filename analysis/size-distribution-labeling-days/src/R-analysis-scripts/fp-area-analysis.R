fp.area.analysis <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 31/01/20
  # Function takes a folder path, and returns the table containing the subclone details for each subclone in the datset being analyzed.
  # Usage:
  # data.path <- c('~/Documents/EXP087-Subclone-size-distribution-measurement/data/processed')
  # df <- fp.color.area.analysis(data.path)
  # Works

  # Returns:
  # A tibble: 6 x 6
  #   N     Colony.ID Treatment Subclone.Color Subclone.Size Subclone.ID
  #   <chr> <chr>     <chr>     <chr>                  <dbl>       <int>
  # 1 N3    01        CTRL      dTomato                289             1
  # 2 N3    01        CTRL      dTomato                115             2
  # 3 N3    01        CTRL      dTomato             114613             3
  # 4 N3    01        CTRL      dTomato               1912             4
  # 5 N3    01        CTRL      dTomato                 13.5           5
  # 6 N3    01        CTRL      dTomato              72573             6

  source('./src/R-tools/shoelace-area-algorithm.R')
  library(tidyverse)

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-tools/shoelace-area-algorithm.R')
    library(tidyverse)
  })


  file.list <- list.files(path = folder.path, pattern = c('Clones-Coordinates.csv'), full.names = TRUE, recursive = TRUE)
  data <- parLapply(cl, file.list, function(x){
    df1 <- read.csv(x)
    df.color <- df1[,-c(1)]
    string <- str_split_fixed(str_extract(x, '\\d{1,3}-[:alnum:]{3,15}-N\\d-[:alpha:]{4,7}'), '-', 4)
    list <- c()
    id <- c()
    for (i in unique(df.color$Number)){
      list[i] <- area(df.color[df.color$Number == i,])
      id[i] <- i
    }
    df2 <- tibble(N = string[3], Colony.ID = string[1], Treatment = string[2], Subclone.Color = string[4], Subclone.Size = list, Subclone.ID = id)
  })
  return (do.call(rbind, data))
}
