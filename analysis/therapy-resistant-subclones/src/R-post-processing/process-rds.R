process.rds <- function(data.path, removal.list.path){
  library(tidyverse)
  data <- readRDS(data.path)
    # Coerce values as numeric for those that aren't yet.
    # 'data.frame':	54353 obs. of  10 variables:
    # $ N               : chr  "N1" "N1" "N1" "N1" ...
    # $ Colony.ID       : chr  3 3 3 3 3 3 3 3 3 3 ...
    # $ Treatment       : chr  "CTRL" "CTRL" "CTRL" "CTRL" ...
    # $ Colony.Size     : num  1568916 1568916 1568916 1568916 1568916 ...
    # $ Subclone.ID     : num  1 2 3 4 5 6 7 8 9 10 ...
    # $ Subclone.Color  : chr  "dTomato" "dTomato" "dTomato" "dTomato" ...
    # $ D.Colony        : num  85.1 31.1 50 0 94.8 ...
    # $ D.Boundary      : num  280.8 21.2 47 0 96 ...
    # $ EdU.per.Subclone: int  1 0 4 22 1 0 1 0 0 0 ...
    # $ Subclone.Size   : num  910 1255 818 5954 944 ...
  '%ni%' <- Negate('%in%')
  # modifications to input dataset here
  data$Colony.ID <- as.numeric(data$Colony.ID)
  data <- data %>% group_by(Colony.ID) %>% mutate(Subclone.Counter = 1:n())
  data$Row <- 1:nrow(data)
  data <- data %>% mutate(Identity = if_else(D.Boundary < 40, "Boundary", if_else(D.Colony < D.EdU.Mean, "Surfing", "Interior")))
  data[data$Treatment == "CTRLBSR",]$Treatment <- "CTRL"
  data[data$Treatment == "CTRLTGFB",]$Treatment <- "TGFBE"
  data <- data %>% group_by(Colony.ID) %>% mutate(Norm.Subclone.Size = Subclone.Size / Colony.Size)
  # Removal of erroneous segmentation
  removal.input <- read.csv(removal.list.path)
  # Defined as a path, read something like this:
  #   Colony.ID Subclone
  # 1         3        2
  # 2         3        3
  # 3         3        4
  # 4         3        5
  # 5         3        6
  # 6       127        1
  # 7       127       19
  row.list <- c()
  for (i in 1:nrow(removal.input)){
      row.list[i] <- data %>% filter(Colony.ID %in% removal.input[i,1]) %>% filter(Subclone.Counter %in% removal.input[i,2]) %>% .$Row
  }

  return(data %>% filter(Row %ni% row.list))
}
