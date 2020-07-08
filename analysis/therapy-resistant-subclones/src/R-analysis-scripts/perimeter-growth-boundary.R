perimeter.growth.boundary <- function(folder.path){
  # Function output
  #   Colony.ID Growth.Per
  # 1         3        600
  # 2       127       1000

  source('./src/R-tools/distance-edu.R')
  source('./src/R-tools/roll.R')
  library("tidyverse")
  library("parallel")

  # Clusterize
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  clusterEvalQ(cl, {
    source('./src/R-tools/distance-edu.R')
    source('./src/R-tools/roll.R')
    library("tidyverse")
    library("parallel")
  })

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

  perimeter.data <- parLapply(cl, colony.bound, function(data){
    output <- data %>%
              group_by(N, Colony.ID, Treatment, Boundary) %>%
              mutate(L = sqrt(abs((X - roll(X,1)))^2 + abs((Y - roll(Y,1)))^2)) %>%
              summarize(P = sum(L))
  })
  perimeter <- do.call(rbind, perimeter.data)
  perimeter$Boundary[perimeter$Boundary == FALSE] <- "P.Free"
  perimeter$Boundary[perimeter$Boundary == TRUE] <- "P.Well"
  perimeter <- perimeter %>% spread(Boundary, P)

  return(perimeter)
}
