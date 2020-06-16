colony.coordinate.preprocessor <- function(folder.path){
  # Takes a folder path, parses each colony and then returns a processed version of the colony coordinates that identifies which of the coordinates are facing the boundary and which are not.
  source('./src/R-tools/distance.R')
  source('./src/R-tools/roll.R')
  library("tidyverse")
  library("parallel")
  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, {
    source('./src/R-tools/distance.R')
    source('./src/R-tools/roll.R')
    library("tidyverse")
    library("parallel")
  })
  # Load Colony and boundary files
  colony.location <- list.files(path = folder.path, pattern = '-Colony-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  boundary.location <- list.files(path = folder.path, pattern = '-Boundary-Coordinates.csv', full.names = TRUE, recursive = TRUE)
  colony <- parLapply(cl, colony.location, function(x){
    a <- read.csv(x)
    b <- a[,-c(1)]
    c <- b[-c(1:3),]
    c$ID <- 1:nrow(c)
    d <- c
  })
  boundary <- parLapply(cl, boundary.location, function(x){
    a <- read.csv(x)
    b <- a[1:512,2:3]
    c <- b
  })
  # Return a list of true or false values if the distance between boundary and colony is less than 30 pixels
  distance.truth.meta <- mcmapply(function(colony, boundary){
    truth.distance <- vector('list', length = nrow(colony))
    for (i in 1:nrow(colony)){
      truth.distance[[i]] <- min.distance(colony$X[i], colony$Y[i], boundary$X, boundary$Y)
    }
    out.distance <- do.call(rbind, truth.distance)
    output <- cbind(colony, out.distance)
  }, colony, boundary, mc.preschedule = TRUE)

  data <- list()
  for (x in 1:ncol(distance.truth.meta)){
    data[[x]] <- as.data.frame(distance.truth.meta[,x])
  }
  final.output <- parLapply(cl, data, function(x){
    a <- x %>% mutate(Boundary = if_else(D > 30, FALSE, TRUE))
  })
  # Save every final.output into the same folder as colony.location
  directories <- lapply(colony.location, function(x){
    dirs <- dirname(x)
  })
  files <- lapply(colony.location, function(x){
    f <- basename(x)
    a <- str_split_fixed(f, '-', 4)
    b <- paste(a[1,1], a[1,2], a[1,3], "Boundary-Coordinates-Processed", sep = '-')
  })
  for (i in 1:length(directories)){
    output_path <- paste(directories[[i]], "/", files[[i]], ".csv", sep='')
    print(output_path)
    write.csv(final.output[[i]], output_path)
  }
}
