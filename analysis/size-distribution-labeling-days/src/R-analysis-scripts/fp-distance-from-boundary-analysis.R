fp.distance.from.boundary.analysis <- function(folder.path){
  # Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 23/05/20
  # Testing for bugs, running on cluster ....

  source('./src/R-functions/distance-boundary-simple.R')
  library("tidyverse")
  library("parallel")

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-functions/distance-boundary-simple.R')
    library("tidyverse")
    library("parallel")
  })

  colony.location <- colony.location <- list.files(path = folder.path, pattern = '-Colony-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  split.location <- str_split_fixed(str_extract(colony.location, '\\d{1,3}-[:alnum:]{1,5}-N\\d'), '-', 3)
  colony.location.index <- tibble(Colony.ID = split.location[,1], Condition = split.location[,2], N = split.location[,3])
  colony.table <- split(colony.location.index, sort(as.numeric(rownames(colony.location.index))))
  colony <- parLapply(cl, colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
  })

  fp.list <- list.files(path = folder.path, pattern = c('Clones-Coordinates.csv'), full.names = TRUE, recursive = TRUE)
  fp.id.list <- str_split_fixed(str_extract(fp.list, '\\d{1,3}-[:alnum:]{1,5}-N\\d-[:alpha:]{3,7}'), '-', 4)
  fp.location.table.tomato <- as_tibble(cbind(location.t = list.files(path = folder.path, pattern = 'dTomato-Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE), id = fp.id.list[fp.id.list[,4] == 'dTomato',][,1]))
  fp.location.table.ypet <- as_tibble(cbind(location.y = list.files(path = folder.path, pattern = 'yPET-Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE), id = fp.id.list[fp.id.list[,4] == 'yPET',][,1]))
  fp.location.table.cfp <- as_tibble(cbind(location.y = list.files(path = folder.path, pattern = 'CFP-Clones-Coordinates.csv', full.names = TRUE, recursive = TRUE), id = fp.id.list[fp.id.list[,4] == 'CFP',][,1]))
  fp.location <- list(fp.location.table.tomato, fp.location.table.cfp, fp.location.table.ypet) %>% reduce(inner_join, by = "id")
  fp <- parApply(cl, fp.location, 1, function(x){
    a <- read.csv(x[1])
    b <- a[,-c(1)]
    c <- read.csv(x[3])
    d <- c[,-c(1)]
    e <- read.csv(x[4])
    f <- e[,-c(1)]
    g <- rbind(b,d,f)
  })

  data.fp <- mcmapply(function(colony, fp, location){
    # Colony Coordinates
      output.tomato <- vector("list", length = length(unique(fp[fp$Color == 'dTomato',]$Number)))
      for (i in unique(fp[fp$Color == 'dTomato',]$Number)){
        fx.list.t <- vector("list", length = length(fp[fp$Color == 'dTomato' & fp$Number == i,]$X))
        for (j in 1:length(fp[fp$Color == 'dTomato' & fp$Number == i,]$X)){
          fx.list.t[[j]] <- min.distance(fp[fp$Color == 'dTomato' & fp$Number == i,]$X[j], fp[fp$Color == 'dTomato' & fp$Number == i,]$Y[j], colony$X, colony$Y, fp[fp$Color == 'dTomato' & fp$Number == i,]$Number[1], "dTomato")
        }
        output.tomato[[i]] <- do.call(rbind, fx.list.t)
      }
      output.ypet <- vector("list", length = length(unique(fp[fp$Color == 'yPET',]$Number)))
      for (i in unique(fp[fp$Color == 'yPET',]$Number)){
        fx.list.y <- vector("list", length = length(fp[fp$Color == 'yPET' & fp$Number == i,]$X))
        for (j in 1:length(fp[fp$Color == 'yPET' & fp$Number == i,]$X)){
          fx.list.y[[j]] <- min.distance(fp[fp$Color == 'yPET' & fp$Number == i,]$X[j], fp[fp$Color == 'yPET' & fp$Number == i,]$Y[j],  colony$X, colony$Y, fp[fp$Color == 'yPET' & fp$Number == i,]$Number[1], "yPET")
        }
        output.ypet[[i]] <- do.call(rbind, fx.list.y)
      }
      output.cfp <- vector("list", length = length(unique(fp[fp$Color == 'CFP',]$Number)))
      for (i in unique(fp[fp$Color == 'CFP',]$Number)){
        fx.list.c <- vector("list", length = length(fp[fp$Color == 'CFP' & fp$Number == i,]$X))
        for (j in 1:length(fp[fp$Color == 'CFP' & fp$Number == i,]$X)){
          fx.list.c[[j]] <- min.distance(fp[fp$Color == 'CFP' & fp$Number == i,]$X[j], fp[fp$Color == 'CFP' & fp$Number == i,]$Y[j],  colony$X, colony$Y, fp[fp$Color == 'CFP' & fp$Number == i,]$Number[1], "CFP")
        }
        output.cfp[[i]] <- do.call(rbind, fx.list.c)
      }
      output.i.tomato <- do.call(rbind, output.tomato)
      output.i.ypet <- do.call(rbind, output.ypet)
      output.i.cfp <- do.call(rbind, output.cfp)
      df.f.tomato <- output.i.tomato %>% group_by(Subclone.ID, Subclone.Color) %>% summarize(D.Colony = min(D.Col), D.Boundary = min(D.Bound))
      df.f.ypet <- output.i.ypet %>% group_by(Subclone.ID, Subclone.Color) %>% summarize(D.Colony = min(D.Col), D.Boundary = min(D.Bound))
      df.f.cfp <- output.i.cfp %>% group_by(Subclone.ID, Subclone.Color) %>% summarize(D.Colony = min(D.Col), D.Boundary = min(D.Bound))
      total.df <- rbind(df.f.tomato, df.f.ypet, df.f.cfp)
      total.df <- total.df %>% mutate(N = location$N, Colony.ID = location$Colony.ID, Treatment = location$Treatment)
  }, colony, fp, colony.table, mc.preschedule = TRUE)

  data <- list()
  for (x in 1:ncol(data.fp)){
    data[[x]] <- as.data.frame(data.fp[,x])
  }

  stopCluster(cl)

  return (do.call(rbind, data))
}
