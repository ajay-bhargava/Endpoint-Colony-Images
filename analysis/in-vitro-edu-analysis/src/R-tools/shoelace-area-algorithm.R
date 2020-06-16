area <- function(df){
  a <- df$X
  b <- df$Y
  add.val <- sub.val <- NA
  for(i in 1:(length(a) - 1)){
    add.val[i] <- 0.5 * a[i] * (b[i + 1])
    sub.val[i] <- 0.5 * b[i] * (a[i + 1])
  }
  return(abs(sum(add.val) - sum(sub.val)))
}
