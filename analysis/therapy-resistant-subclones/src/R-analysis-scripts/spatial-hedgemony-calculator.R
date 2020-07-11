spatial.hedgemony.calculator <- function(folder.path){
  source('./src/R-tools/distance-between-objects.R')
  library("tidyverse")
  library("parallel")

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-tools/distance-between-objects.R')
    library("tidyverse")
    library("parallel")
  })

  colony.location <- list.files(path = folder.path, pattern = '-Boundary-Coordinates-Processed.csv', full.names = TRUE, recursive = TRUE)
  split.location <- str_split_fixed(str_extract(colony.location, '\\d{1,3}-[:alnum:]{3,15}-N\\d'), '-', 3)
  colony.location.index <- tibble(N = split.location[,3], Colony.ID = split.location[,1], Treatment = split.location[,2])
  colony.table <- split(colony.location.index, sort(as.numeric(rownames(colony.location.index))))
  colony <- parLapply(cl, colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
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

  data.fp <- mcmapply(function(colony, fp, location){
    # Colony Coordinates
      output.tomato <- vector("list", length = length(unique(fp[fp$Color == 'dTomato',]$Number)))
      for (i in unique(fp[fp$Color == 'dTomato',]$Number)){
        fx.list.t <- vector("list", length = length(fp[fp$Color == 'dTomato' & fp$Number == i,]$X))
        for (j in 1:length(fp[fp$Color == 'dTomato' & fp$Number == i,]$X)){
          fx.list.t[[j]] <- min.distance(fp[fp$Color == 'dTomato' & fp$Number == i,]$X[j], fp[fp$Color == 'dTomato' & fp$Number == i,]$Y[j], colony, fp[fp$Color == 'dTomato' & fp$Number == i,]$Number[1], "dTomato")
        }
        output.tomato[[i]] <- do.call(rbind, fx.list.t)
      }
      output.ypet <- vector("list", length = length(unique(fp[fp$Color == 'yPET',]$Number)))
      for (i in unique(fp[fp$Color == 'yPET',]$Number)){
        fx.list.y <- vector("list", length = length(fp[fp$Color == 'yPET' & fp$Number == i,]$X))
        for (j in 1:length(fp[fp$Color == 'yPET' & fp$Number == i,]$X)){
          fx.list.y[[j]] <- min.distance(fp[fp$Color == 'yPET' & fp$Number == i,]$X[j], fp[fp$Color == 'yPET' & fp$Number == i,]$Y[j], colony, fp[fp$Color == 'yPET' & fp$Number == i,]$Number[1], "yPET")
        }
        output.ypet[[i]] <- do.call(rbind, fx.list.y)
      }
      output.i.tomato <- do.call(rbind, output.tomato)
      output.i.ypet <- do.call(rbind, output.ypet)
      df.f.tomato <- output.i.tomato %>% group_by(Subclone.ID, Subclone.Color) %>% filter(D.Col < 50)
      df.f.ypet <- output.i.ypet %>% group_by(Subclone.ID, Subclone.Color) %>% filter(D.Col < 50)
      total.df <- rbind(df.f.tomato, df.f.ypet)
      total.df <- total.df %>% mutate(N = location$N, Colony.ID = location$Colony.ID, Treatment = location$Treatment)
  }, colony, fp, colony.table, mc.preschedule = TRUE)

  data <- list()
  for (x in 1:ncol(data.fp)){
    data[[x]] <- as.data.frame(data.fp[,x])
  }

  stopCluster(cl)

  return (do.call(rbind, data))
}
