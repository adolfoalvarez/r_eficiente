diagonal <- function(n, d1, d2){
  matrix(c(
    c(d1,d2,rep(0, n-2)),
    rep(c(d2,d1,d2, rep(0,n-2)), n-2),
    c(d2,d1)
  ), 
  ncol=n, byrow = T)
}