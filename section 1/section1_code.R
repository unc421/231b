##############################################################
# 231b Section 1. UC Berkeley, Spring 2019.
# GSI: Christopher Carter christopher.carter@berkeley.edu
# 
##############################################################

## -------------------------------------------------------------
# libraries
library(png)
library(grid)
library(ggplot2)
library(xtable)


## ------------------------------------------------------------------------
conservative <- c(4,5,7,10,7,8,9,3,5,8,
                  9,3,2,3,4,6,10,4,6,7,8,9,2)

N <- length(conservative) # number of observations in the population

N

pop_mean <- mean(conservative) # population mean
pop_mean 

pop_sd <- sd(conservative) # population standard deviation
pop_sd

## ------------------------------------------------------------------------

##Set a seed for replication
set.seed(12345)

s1 <- sample(conservative, size=8, replace = FALSE)

s2 <- sample(conservative, size=8, replace = FALSE)

s3 <- sample(conservative, size=8, replace = FALSE)

s4 <- sample(conservative, size=8, replace = FALSE)

samples <- rbind(s1, s2, s3, s4)

samples

## ------------------------------------------------------------------------
apply(samples, MARGIN=1, FUN=mean) 

##Compare to the population mean

pop_mean

## ------------------------------------------------------------------------

sample_mean <- NA

for (i in 1:10000){
  
  sample <- sample(conservative, size=8, replace = FALSE)
  sample_mean[i] <- mean(sample)
  
}

## ------------------------------------------------------------------------
par(mfrow=c(1,1))
plot(density(sample_mean), col = "blue", lwd = 3,
     main = "Distribution of sample means")
abline(v = pop_mean, col = "red", lwd = 2)


## -------------------------------------------------------------
 
 rep <- 10000
 
 # The first loop varies m
 for (n in 9:20){
 
   sample_mean <- NA #creating an object to store the results of the second loop
 
   # The second loop goes through the 10000 simulations
   for (i in 1:rep){
 
     #we first get a random sample of size m from the population
     sample <- sample(conservative, size=n, replace = FALSE)
     #and then calculate and store the sample mean
     sample_mean[i] <- mean(sample)
   }
 
   #finally, we plot the distribution of the 10000 sample means for the relevant n
   lines(density(sample_mean), lwd=3,
         
         #note that this next line of code varies the color of the line according to n
         
         #so that we can distinguish the different distributions
         col=paste0("grey",140-(7*n)))
   
 }
 

## -------------------------------------------------------------

plot(density(sample_mean), col="blue", ylim=c(0,1.6),
     main="Distribution of sample means", lwd=3)
abline(v=pop_mean, col="red", lwd=3)

rep <- 10000

for (n in 9:20){
  sample_mean <- NA
  
  for (i in 1:rep){
    sample <- sample(conservative, size=n, replace = FALSE)
    sample_mean[i] <- mean(sample)
  }
  
  lines(density(sample_mean), lwd=3,
        col=paste0("grey",140-(7*n)))
}


## -------------------------------------------------------------

village <- seq(1,10)
  
program <- c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1)

conservative <- c(5, 6, 7, 4, 3, 6, 2, 9, 2, 2)

cbind.data.frame(village, program, conservative)



## ------------------------------------------------------------------------
mean(conservative[program == 1], na.rm = TRUE) - 
  mean(conservative[program == 0], na.rm = TRUE)

## ------------------------------------------------------------------------

diff_means <- function(outcome, treatment){ 
  
  # Calculating difference in means
  mean1 <- mean(outcome[treatment==1], na.rm=T)
  mean0 <- mean(outcome[treatment==0], na.rm=T)
  diff <- mean1 - mean0
  
  # Calculating number of observations
  N <- length(na.omit(outcome))
  
  # Preparing output
  res <- c(mean1, mean0, diff, N)
  names(res) <- c("Mean 1", "Mean 0", "Difference", "N")
  
  return(c(res))
}

## ------------------------------------------------------------------------

diff_means(conservative, program)


## -----------------------------------------
print(xtable(cbind.data.frame(village, program, 
                              conservative), digits = 0), 
      include.rownames = FALSE, 
      type = "html")


