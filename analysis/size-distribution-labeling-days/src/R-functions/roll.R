roll <- function( x , n ){
  if( n == 0 )
    return( x )
  c( tail(x,n) , head(x,-n) )
}
