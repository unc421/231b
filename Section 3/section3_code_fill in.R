
rm(list=ls())


##Set libraries

library(MASS)
library(car)
library(rgl)
library(scatterplot3d)
library(ggplot2)



## ------------------------------------------------------------------------
gg_data <- as.data.frame(cbind(c(10,15,20,20,10,15,15), 
                               c(15,15,30,15,20,15,30)))
names(gg_data) <- c("Y_i0", "Y_i1")
# generating treatment vector for a given experiment
gg_data$treat <- c(1, 0, 0, 0, 0, 0, 1)

# getting observed outcomes
gg_data$observed <- ifelse(gg_data$treat==1, gg_data$Y_i1, gg_data$Y_i0)

# ate
ATE <- mean(gg_data$observed[gg_data$treat==1]) - mean(gg_data$observed[gg_data$treat==0])
ATE


## ------------------------------------------------------------------------
treated <- gg_data$observed[gg_data$treat==1]
treated

var1 <- sum((treated - mean(treated))^2) / (length(treated) - 1)
var1

not_treated <- gg_data$observed[gg_data$treat==0]
not_treated

var0 <- sum((not_treated - mean(not_treated))^2) / (length(not_treated) - 1)
var0

estimated_se <- sqrt(var1/length(treated) + var0/length(not_treated))

estimated_se 


## ------------------------------------------------------------------------

# converting to standard units (how do we do that and why?)
t_stat <- 

# To get the correct Student t Distribution, we need to calculate
# the degrees of freedom (Satterthwaite)
df <- (var1/length(treated) + var0/length(not_treated))^2 / 
           ((var1/length(treated))^2 / (length(treated) - 1) + 
           (var0/length(not_treated))^2 / (length(not_treated) - 1))
df


## ------------------------------------------------------------------------

# Overlaying the t_stat to the student t distribution
ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun=dt, args=list(df=df, ncp=0), col="blue", size=1) +
  geom_vline(xintercept=mean(t_stat), col="red", size=1.25) 

## ------------------------------------------------------------------------
# One tailed p-value (what does lower.tail mean?)
pt(t_stat, df=df, ncp=0, lower.tail=F)

# Two tailed p-value?


## ------------------------------------------------------------------------
fake_treats <- matrix(NA, 10000, 7)
for (i in 1:10000){
fake_treats[i,] <- sample(gg_data$treat, 7, replace=F)
}


## ------------------------------------------------------------------------

fake_treats <- unique(fake_treats)


## ------------------------------------------------------------------------
rand_ate <- NA # placeholder vector for results

for (i in 1:nrow(fake_treats)){ # for each of the fake treatment vectors
  
  mean_treat <- mean(gg_data$observed[fake_treats[i,]==1])
  
  mean_control <- mean(gg_data$observed[fake_treats[i,]==0])
  
  # calculating ATE for this randomization
  rand_ate[i] <- mean_treat - mean_control
  
}


## Randomization inference

m <- ggplot(as.data.frame(rand_ate), aes(x=rand_ate))
m + 
  geom_histogram(aes(y = ..density..), binwidth=.5) +
  geom_vline(xintercept=ATE, col="red", size=1) +
  theme_bw() +
  xlab("Randomization ATEs") +
  ylab("Density")


## ------------------------------------------------------------------------

# What's the difference between the next two lines?

sum(rand_ate>=ATE)/length(rand_ate)

sum(abs(rand_ate)>=ATE)/length(rand_ate)



## Linear regression
##---------------------------------------------------------

load(url("http://www.stat.berkeley.edu/users/nolan/data/stat133/family.rda"))

## Let's find the dimensions

dim(family)
family


###############################################
##Now we're going to write some functions!!!!##
###############################################

## This function takes two variables as input and returns their correlation. How would you write it? What's our formula?
r <- function(x, y){
 }

## ------------------------------------------------------------------------
# Correlation function using sum, mean, and length
r <- function(x, y) {
  # this function takes two variables as input and returns their correlation.
  
  # lengths
  n_x = length(x)
  n_y = length(y)
  
  # means  
  mean_x = mean(x)
  mean_y = mean(y)
  
  # sd_x, with no df correction
  sd_x = sqrt(sum((x - mean_x)^2) / (n_x))
  # sd_y, with no df correction
  sd_y = sqrt(sum((y - mean_y)^2) / (n_y))
  
  # cov(x, y)
  cov_x_y = mean((x - mean_x)*(y - mean_y))
  
  # cov(x, y) / (sd_x * sd_y)
  cov_x_y / (sd_x * sd_y)
}


## ------------------------------------------------------------------------
r <- function(x, y){
  # This function takes two variables as input and returns their correlation.
  
  sx <- sqrt(mean((x-mean(x))^2))
  sy <- sqrt(mean((y-mean(y))^2))
  
  
  r <- sum(((x - mean(x))/sx) * ((y - mean(y)) / sy)) / (length(x))
  
  return(r)

}


## ------------------------------------------------------------------------
# variables
x <- family$height
y <- family$weight

# sds
sx <- sqrt(mean((x-mean(x))^2))
sy <- sqrt(mean((y-mean(y))^2))

# coefficients
b_hat <- r(x,y) * (sy/sx)
a_hat <- mean(y) - b_hat*mean(x)

c(a_hat, b_hat)

## This function takes two variables as input and returns regression coefficients predicting the first variable from the second.

regcoef <- function(y, x){
 
  }

## Let's test it!

regcoef(y=family$weight, x=family$height)

##And see if it gets back the canned function?
lm(family$weight ~ family$height)


## This function plots a scatterplot with the regression line in red.

regline <- function(y, x){
 }

##Let's test it!
with(family, regline(fheight, fweight))





## What does this line do?

lines(x, (coefs[1]+coefs[2]*x), col="red", lwd=2)


## Now let's make a plot where the size of our points vary by a third variable

par(mfrow=c(1,1))

size <- (family$bmi)^2/sum(family$bmi) 

plot(family$height, family$weight, pch=16, cex=size, col="grey10")
abline(lm(family$weight ~ family$height), col="red", lwd=2)



###############################
### Multivariate Regression ###
###############################

##What is our formula for finding \hat{beta}? Let's imagine we care about the relationship between X (height and BMI) and Y (weight)

Y <- ##Define Y

Y


## ------------------------------------------------------------------------

X <- ##Define X

dim(X)

X


## How do we find beta hat?

beta_hat <- ##Find beta hat

## Check against canned function

lm(Y~X[,2:3])




