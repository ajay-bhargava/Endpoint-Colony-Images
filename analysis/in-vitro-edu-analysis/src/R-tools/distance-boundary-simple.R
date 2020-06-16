min.distance <- function(i,j,p,q,n,c){
  # Takes an input i, j which are single numbers, and returns the minimum euclidean distance between points on each item on vector p, and q
  a <- (p-i)^2 + (q-j)^2
  # Need to specify here, if a distance value
  return (data.frame(D = sqrt(min(a)), Subclone.ID = n, Subclone.Color = c))
}
