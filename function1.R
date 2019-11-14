function1 <- function(v){
  retval <- 0
  for (i in 1:length(v)){
    retval = retval + v[i]*v[i]
    #pause(0.1)
  }
  return(sqrt(retval))
}