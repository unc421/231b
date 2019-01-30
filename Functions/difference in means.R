diff_means <- function(y, x){ 
  
  # Calculating difference in means
  mean1 <- mean(y[x==1], na.rm=T)
  mean0 <- mean(y[x==0], na.rm=T)
  diff <- mean1 - mean0
  
  # Calculating number of observations
  N <- length(na.omit(y))
  
  # Preparing output
  res <- c(mean1, mean0, diff, N)
  names(res) <- c("Mean 1", "Mean 0", "Difference", "N")
  
  return(c(res))
}
