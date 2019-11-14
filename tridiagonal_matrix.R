tridiagonal_matrix <- function(n, d1, d2){
  main_diag <- diag(n) * d1
  lower_diag <- cbind(diag(n)[,c(2:n)],rep(0,n)) * d2
  upper_diag <- rbind(diag(n)[c(2:n),],rep(0,n)) * d2
  return(main_diag + lower_diag + upper_diag)
}