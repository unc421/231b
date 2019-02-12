

rm(list=ls())
set.seed(94705)

# Libraries
library(MASS)

# sourcing plotting functions
functions <-  "https://raw.githubusercontent.com/unc421/231b/master/Functions/functions.R"
source(functions)

#generating variation in x1 and x2 
#(note this does not mean we are treating them as random variables).
x1 <- runif(80, -50, 50)
x2 <- rnorm(80, 5, 20)

epsilon <- rnorm(80, 0, 16)

Y <- 14 + 8 * x1 + 3 * x2 + epsilon



# The first thing we will need here is to build the matrix for X
X <- 

## ------------------------------------------------------------------------
betas <- 

e <- Y - X %*% betas


## ------------------------------------------------------------------------
round(mean(e), 4)

hat_sigma2 <- 
hat_sigma <- sqrt(hat_sigma2)

hat_sigma

## ------------------------------------------------------------------------

var_hat_beta <- hat_sigma2 * solve((t(X)%*%X)) # Why do we use * instead of %*% here?
var_hat_beta 

## ------------------------------------------------------------------------
se_hat_beta <- sqrt(diag(var_hat_beta))
se_hat_beta


## ------------------------------------------------------------------------

t_stats <- 
t_stats

for (i in 1:3){
  
  p_val <- pt(abs(t_stats[i]), df=(nrow(X)-length(betas)), ncp=0, lower.tail = F) + 
    pt(-abs(t_stats[i]), df=(nrow(X)-length(betas)), ncp=0, lower.tail = T)
  
  print(p_val)
}

## ------------------------------------------------------------------------

ovb_sim <- function(X, betas, e, omitted=FALSE){
  
  # Create the simulated y by
  # adding together the systematic and stochastic
  # components, according to the true model
  # note that we are adding column of 1s for the intercept
  
  y <- cbind(1, X) %*% betas + sample(e, 100, replace=F)
  
  # Run a regression of the simulated y on the simulated X with the option of omitting x2 or not
 
  
  # Extract the estimated coefficients
  
  coefs <- 
  
  # Return the coefficients
  return(coefs)
}

## ------------------------------------------------------------------------

# True variance-covariance matrices
# Here the off-diagonal elements are zero, so they variables are not correlated
# with each other
SigmaX_without <- matrix(c( 2,  0,  0,
                            0,  2,  0,
                            0,  0,  2 ), 
                         nrow=3, ncol=3, byrow=TRUE)

# Now in these the off-diagonal elements are non-zero (the variables are correlated)
# 
SigmaX_with_positive <- matrix(c( 1,  .5,  .5,
                                 .5,   1,  .5,
                                 .5,  .5,   1 ), 
                               nrow=3, ncol=3, 
                               byrow=TRUE)

SigmaX_with_negative <- matrix(c(  1,  -.5,  -.5,
                                 -.5,    1,  -.5,
                                 -.5,  -.5,    1 ), 
                               nrow=3, ncol=3, 
                               byrow=TRUE)



## ------------------------------------------------------------------------

# Draw the simulated covariates from their true
# multivariate Normal distribution
X_without <- mvrnorm(n=200, mu=c(0,0,0), 
                     Sigma=SigmaX_without)

X_with_positive <- mvrnorm(n=200, mu=c(0,0,0), 
                           Sigma=SigmaX_with_positive)

X_with_negative <- mvrnorm(n=200, mu=c(0,0,0), 
                           Sigma=SigmaX_with_negative)

# and the vector for the error term
error <- rnorm(200)*2




## ------------------------------------------------------------------------
reg1 <- replicate(10000, ovb_sim(X=X_with_positive, 
                                 e=error,
                                 betas=c(1,2,4,3), 
                                 omitted=FALSE))

## ------------------------------------------------------------------------
reg2 <- replicate(10000, ovb_sim(X=X_with_positive, 
                                 e=error,
                                 betas=c(1,2,4,3), 
                                 omitted=TRUE))

## ------------------------------------------------------------------------
reg3 <- replicate(10000, ovb_sim(X=X_with_negative, 
                                 e=error,
                                 betas=c(1,2,4,3), 
                                 omitted=TRUE))

## ------------------------------------------------------------------------
reg4 <- replicate(10000, ovb_sim(X=X_without, 
                                 e=error,
                                 betas=c(1,2,4,3),  
                                 omitted=TRUE))

## ------------------------------------------------------------------------

# Average OLS estimate across 10000 simulation runs: 
apply(reg1, 1, mean) # correlated covariates but complete model

apply(reg2, 1, mean) # positively correlated covariates and omitted variable

apply(reg3, 1, mean) # negatively correlated covariates and omitted variable

apply(reg4, 1, mean) # non-correlated covariates and omitted variable


## ---- echo=FALSE, fig.height=5-------------------------------------------
plotfunc(coefs=reg1, betas=c(1,2,4,3), omitted=FALSE, 
         maintitle="Complete model")


## ---- echo=FALSE, fig.height=5-------------------------------------------

plotfunc(coefs=reg2, betas=c(1,2,4,3), 
         omitted=TRUE, 
         maintitle="Omitted variable 
         (positive covariance)")


## ---- echo=FALSE, fig.height=5-------------------------------------------

plotfunc(coefs=reg3, betas=c(1,2,4,3), omitted=TRUE, 
         maintitle="Omitted variable (negative covariance)")

## ---- echo=FALSE, fig.height=5-------------------------------------------

plotfunc(coefs=reg4, betas=c(1,2,4,3), omitted=TRUE, 
         maintitle="Omitted variable (zero correlation)")



