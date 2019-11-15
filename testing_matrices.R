#Generate 100 matrices where n = 500, and d1, and d2 are predefined random integers between -10 and 10

d1_vector <- sample(-10:10, 100, replace = T)
d2_vector <- sample(-10:10, 100, replace = T)
# d1_vector <- round(runif(100, -10, 10))
# d2_vector <- round(runif(100, -10, 10))

#Our function
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

library(purrr)
# For cycle
system.time({matrices1 <- vector(mode = "list", length = length(d1_vector))
# matrices <- list()
for (i in 1:length(d1_vector)){
  matrices1[[i]] <- create_tridiagonal2(500, d1 = d1_vector[i], d2 = d2_vector[i])
}
})

# lapply
tridiagonal_i <- function(i) {
  create_tridiagonal2(n = 500,
                      d1 = d1_vector[i],
                      d2 = d2_vector[i])
}
system.time({
  matrices2 <- lapply(seq_along(d1_vector),tridiagonal_i)
  
})

system.time({
  matrices3 <- map(seq_along(d1_vector),tridiagonal_i)
  
})

#mapply

system.time({
  matrices3.5 <- mapply(create_tridiagonal2, d1 = d1_vector,
                        d2 = d2_vector, n = 500)
})

#map2
system.time({
  matrices4 <- map2(d1_vector, d2_vector, create_tridiagonal2, n = 500)
})

#Parallel

library(furrr)
plan(multisession)
plan(multicore)
system.time({
  matrices5 <- future_map2(d1_vector, d2_vector, create_tridiagonal2, n = 500)
})

system.time({
  matrices6 <-future_map(seq_along(d1_vector),
                   function(i) {
                     create_tridiagonal2(n = 500,
                                         d1 = d1_vector[i],
                                         d2 = d2_vector[i])
                   })
})

myfunction <- function(i) {
  create_tridiagonal2(n = 500,
                      d1 = d1_vector[i],
                      d2 = d2_vector[i])}
library(rbenchmark)
benchmark(matrices6 <- future_map(seq_along(d1_vector), myfunction),
          matrices7 <- map(seq_along(d1_vector), myfunction),
          replications=3)

benchmark(matrices3 <- map2(d1_vector, d2_vector, create_tridiagonal2, n = 500),
          matrices4 <- future_map2(d1_vector, d2_vector, create_tridiagonal2, n = 500),
          replications = 3)

system.time({
  matrices2 <- lapply(seq_along(d1_vector),myfun)
  
})
system.time({
  matrices2 <- parallel::mclapply(seq_along(d1_vector),myfun, mc.cores = 8)
  
})

# <- mclapply(1:10, function(i) {
#   +         Sys.sleep(10)  ## Do nothing for 10 seconds
#   + }, mc.cores = 10)      ## Split this job across 10 cores

### TRY TO USE GPU MATRIX 

#Other parallel versions
library(foreach)
cl <- parallel::makeForkCluster(8)
doParallel::registerDoParallel(cl)
#length(d1_vector)
system.time({
a <- foreach(i = 1:length(d1_vector)) %dopar% {
  create_tridiagonal2(500, d1=d1_vector[i], d2 = d2_vector[i])
}
})
a## [1] 1.000000 1.414214 1.732051

parallel::stopCluster(cl)
