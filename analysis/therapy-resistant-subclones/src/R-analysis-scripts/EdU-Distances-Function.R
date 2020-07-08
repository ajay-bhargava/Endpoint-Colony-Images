edu.distance.boundary <- function(folder.path){
  # Function output
  #   Colony.ID mean.EdU.D
  # 1         3        600
  # 2       127       1000
  # To be joined with rds file

  source('./src/R-tools/distance-edu.R')
  library("tidyverse")
  library("parallel")

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-tools/distance-edu.R')
    library("tidyverse")
    library("parallel")
  })

  # Load files for EdU
  edu.list <- list.files(path = folder.path, pattern = '-EdU-Maxima.csv', full.names = TRUE, recursive = TRUE)
  edu.split.location <- str_split_fixed(str_extract(edu.list, '\\d{1,3}-[:alnum:]{3,15}-N\\d'), '-', 3)
  edu.split.index <- tibble(N = edu.split.location[,3], Colony.ID = edu.split.location[,1], Treatment = edu.split.location[,2])
  edu.table <- split(edu.split.index, sort(as.numeric(rownames(edu.split.index))))
  edu <- parLapply(cl, edu.list, function(x){
    a <- read.csv(x)
    b <- distinct(a, Number, .keep_all = TRUE)
    c <- b[,-c(1:2)]
  })

  edu.bound <- vector('list', length = length(edu))
  for (i in 1:length(edu)){
    edu.bound[[i]] <- cbind(edu.table[[i]], edu[[i]])
  }

  # Load files for Colony
  colony.location <- list.files(path = folder.path, pattern = '-Boundary-Coordinates-Processed.csv', full.names = TRUE, recursive = TRUE)
  split.location <- str_split_fixed(str_extract(colony.location, '\\d{1,3}-[:alnum:]{3,15}-N\\d'), '-', 3)
  colony.location.index <- tibble(N = split.location[,3], Colony.ID = split.location[,1], Treatment = split.location[,2])
  colony.table <- split(colony.location.index, sort(as.numeric(rownames(colony.location.index))))
  colony <- parLapply(cl, colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
  })

  colony.bound <- vector('list', length = length(colony))
  for (i in 1:length(colony)){
    colony.bound[[i]] <- cbind(colony.table[[i]], colony[[i]])
  }

  distances.meta <- mcmapply(function(edu, colony){
    edu.distance <- vector('list', length = nrow(edu))
    for (i in 1:nrow(edu)){
      edu.distance[[i]] <- distance.edu(edu$X[i], edu$Y[i], colony)
    }
    edu.final <- do.call(rbind, edu.distance)
    output <- cbind(edu.final, edu)
  }, edu.bound, colony.bound, mc.preschedule = TRUE)

  data <- list()
  for (x in 1:ncol(distances.meta)){
    data[[x]] <- as.data.frame(distances.meta[,x])
  }
  return(do.call(rbind, data))
}
