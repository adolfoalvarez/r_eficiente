create_tridiagonal2 <- function(n = 5, d1 = 1, d2 = 2) {
  #We create the matrix and assign the diagonal in one step
  output_matrix <- diag(d1, n)
  
  #"row" is a function in base r, so I prefer "i" as counter
  # We put the d2 value in one step instead of 2
  for (i in 1:(n - 1)) {
    output_matrix[i, c(i - 1, i + 1)] <- d2
  }
  output_matrix[c(i - 1, i + 1), i] <- d2
  return(output_matrix)
}
