#' Geometric Mean
#'
#' Computes geometric mean of a vector of numbers
#'
#'@export
#'@param x vector of numbers without NAs
geometric_mean <- function( x ) {
  argCheck(x)
  n <- length(x)
 gm <- prod(x)^(1/n) 
 return(gm)
}

harmonic_mean <- function( x ) { 
  argCheck(x)
  n <- length(x)
  hm <- n / sum( reciprocal(x))
  return( hm) 
}

reciprocal <- function( x ) {
  if( max( x ==0) > 0 ){ stop("zeros not allowed in x")}
  recip <- 1/x
  return( recip ) 
}

argCheck <- function( x ) {
  if( max( is.na(x) ) > 0 ) { stop("NA values not allowed")}
}
 


