tridiagonal_rowcol <- function(n, d1, d2)
{
  x <- diag(x = d1, n, n) 
  x[row(x)-1 == col(x)] <- d2
  x[row(x) == col(x)-1] <- d2
  return(x)
}