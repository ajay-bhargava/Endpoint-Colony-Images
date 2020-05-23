fp.area.analysis <- function(folder.path){
  # Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 23/05/20
  # Build Complete

  source('./analysis/size-distribution-labeling-days/src/R-functions/shoelace-area-algorithm.R')
  library(tidyverse)

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./analysis/size-distribution-labeling-days/src/R-functions/shoelace-area-algorithm.R')
    library(tidyverse)
  })


  file.list <- list.files(path = folder.path, pattern = c('Clones-Coordinates.csv'), full.names = TRUE, recursive = TRUE)
  data <- parLapply(cl, file.list, function(x){
    df1 <- read.csv(x)
    df.color <- df1[,-c(1)]
    string <- str_split_fixed(str_extract(x, '\\d{1,3}-[:alnum:]{1,5}-N\\d-[:alpha:]{3,7}'), '-', 4)
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
