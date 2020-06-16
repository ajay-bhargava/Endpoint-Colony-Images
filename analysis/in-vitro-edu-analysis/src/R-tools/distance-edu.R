distance.edu <- function(i,j,colony){
  if (nrow(filter(colony, Boundary == TRUE)) == 0){
    b <- NA
  } else {
    data <- filter(colony, Boundary == TRUE)
    b <- (data$X-i)^2 + (data$Y-j)^2
  }

  if (nrow(filter(colony, Boundary == FALSE)) == 0){
    a <- NA
  } else {
    object <- filter(colony, Boundary == FALSE)
    a <- (object$X-i)^2 + (object$Y-j)^2
  }
  return (data.frame(D.Free = sqrt(min(a)), D.Well = sqrt(min(b))))
}
