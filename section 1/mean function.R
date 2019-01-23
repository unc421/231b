##Mean function

average <- function(x){
  
  ##Code average
 ave_x <- sum(x)/length(x)
  
 
 ##Create label for output

  names(ave_x) <- "Mean of x"
  
  ##Return average with label
  
  return(ave_x)
  
}


##Test mean function

#Using data from section

conservative <- c(4,5,7,10,7,8,9,3,5,8,9,
                  3,2,3,4,6,10,4,6,7,8,9,2)


average(conservative)

mean(conservative)

