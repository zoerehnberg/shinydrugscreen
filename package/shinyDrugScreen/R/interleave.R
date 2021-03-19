# function to only label every other tick mark
interleave <- function(x,y){
  lx <- length(x)
  ly <- length(y)
  n <- max(lx,ly)
  return(as.vector(rbind(rep(x, length.out=n), rep(y, length.out=n))))
}