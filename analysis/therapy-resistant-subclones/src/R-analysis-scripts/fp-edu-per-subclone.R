fp.edu.per.subclone <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 05/02/20
  # Function takes a folder path, and returns a table that has got the distance from the boundary for each subclone
  # Usage:
  # folder.path <- c('~/Documents/EXP087-Subclone-size-distribution-measurement/data/processed')
  # df <- fp.edu.per.subclone(folder.path)
  # Works

  # Load subclone files, then process the files down to a list of FP table.
  library("tidyverse")
  library("sp")
  library("parallel")
  source('./src/R-tools/spatial-overlay-calculator.R')

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-tools/spatial-overlay-calculator.R')
    library("tidyverse")
    library("parallel")
    library("sp")
  })

  fp.list <- list.files(path = folder.path, pattern = 'Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  fp.id.list <- str_split_fixed(str_extract(fp.list, '\\d{1,3}-[:alnum:]{3,15}-N\\d-[:alpha:]{4,7}'), '-', 4)
  fp.location.table.tomato <- as_tibble(cbind(location.t = list.files(path = folder.path, pattern = 'dTomato-Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE), id = fp.id.list[fp.id.list[,4] == 'dTomato',][,1]))
  fp.location.table.ypet <- as_tibble(cbind(location.y = list.files(path = folder.path, pattern = 'yPET-Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE), id = fp.id.list[fp.id.list[,4] == 'yPET',][,1]))
  fp.location <- data.frame(merge(fp.location.table.tomato, fp.location.table.ypet))
  fp <- parApply(cl, fp.location, 1, function(x){
    a <- read.csv(x[3])
    b <- a[,-c(1)]
    c <- read.csv(x[2])
    d <- c[,-c(1)]
    c <- rbind(b,d)
  })

  edu.list <- list.files(path = folder.path, pattern = '-EdU-Maxima.csv', full.names = TRUE, recursive = TRUE)
  edu.split.location <- str_split_fixed(str_extract(edu.list, '\\d{1,3}-[:alnum:]{3,15}-N\\d'), '-', 3)
  edu.split.index <- tibble(N = edu.split.location[,3], Colony.ID = edu.split.location[,1], Treatment = edu.split.location[,2])
  edu.location.mapply.list <- split(edu.split.index, sort(as.numeric(rownames(edu.split.index))))
  edu <- parLapply(cl, edu.list, function(x){
    a <- read.csv(x)
    b <- distinct(a, Number, .keep_all = TRUE)
    c <- b[,-c(1:2)]
  })

  data.output <- mcmapply(function(id, fp, edu){
    edu.points <- SpatialPoints(cbind(edu$X, edu$Y))
    dtomato.edu.output <- list()
    for (i in unique(fp[fp$Color == 'dTomato',]$Number)){
      dtomato.edu.output[[i]] <- spatial.overlay.calculator(edu.points, fp[fp$Color == 'dTomato' & fp$Number == i,], fp[fp$Color == 'dTomato' & fp$Number == i,]$Color[1], fp[fp$Color == 'dTomato' & fp$Number == i,]$Number[1])
    }
    dtomato.list <- do.call(rbind, dtomato.edu.output)
    yPET.edu.output <- list()
    for (i in unique(fp[fp$Color == 'yPET',]$Number)){
      yPET.edu.output[[i]] <- spatial.overlay.calculator(edu.points, fp[fp$Color == 'yPET' & fp$Number == i,], fp[fp$Color == 'yPET' & fp$Number == i,]$Color[1], fp[fp$Color == 'yPET' & fp$Number == i,]$Number[1])
    }
    yPET.list <- do.call(rbind, yPET.edu.output)
    data.list <- rbind(dtomato.list, yPET.list)
    data.out <- data.list %>% mutate(N = id$N, Colony.ID = id$Colony.ID, Treatment = id$Treatment)

  },edu.location.mapply.list, fp, edu, mc.preschedule = TRUE)

  data <- list()
  for (x in 1:ncol(data.output)){
    data[[x]] <- as.data.frame(data.output[,x])
  }

  return (do.call(rbind, data))
}
