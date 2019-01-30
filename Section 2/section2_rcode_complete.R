## ---- echo=FALSE---------------------------------------------------------

rm(list=ls())

# Loading needed libraries
library(foreign) # to import csv
library(ggplot2) # for plots using ggplot




## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
gg_data <- as.data.frame(cbind(c(10,15,20,20,10,15,15), 
                               c(15,15,30,15,20,15,30)))
names(gg_data) <- c("Y_i0", "Y_i1")
save(gg_data, file="gg_data.Rda")

## ------------------------------------------------------------------------
# let's fix m=3 (units in the treatment group)
treat <- c(1, 1, 1, 0, 0, 0, 0)
gg_data$treat <- sample(treat, 7, replace=F)
gg_data$treat

## ------------------------------------------------------------------------
gg_data$observed <- ifelse(gg_data$treat==1, gg_data$Y_i1, gg_data$Y_i0)

## ------------------------------------------------------------------------
head(gg_data)

## ------------------------------------------------------------------------
# mean of the treatment group
mean(gg_data$observed[gg_data$treat==1])
# mean of the control group
mean(gg_data$observed[gg_data$treat==0])

# difference of means
mean(gg_data$observed[gg_data$treat==1]) - mean(gg_data$observed[gg_data$treat==0])
  
# with our function
diff_means(gg_data$observed, gg_data$treat)


## ------------------------------------------------------------------------

# 1.
gg_data$treat <- sample(treat, 7, replace=F)
gg_data$observed <- ifelse(gg_data$treat==1, gg_data$Y_i1, gg_data$Y_i0)

# 2.
diff_means(gg_data$observed, gg_data$treat)
# we should store this! so,
dm <- diff_means(gg_data$observed, gg_data$treat)
dm
# but we only want the third element!
dm <- diff_means(gg_data$observed, gg_data$treat)[3]
dm


## ------------------------------------------------------------------------

dm <- NA #creating a placeholder to store all our doms...

for (i in 1:10000){
    
    # 1.
    gg_data$treat <- sample(treat, 7, replace=F)
    gg_data$observed <- ifelse(gg_data$treat==1, gg_data$Y_i1, gg_data$Y_i0)
    
    # 2.
    dm[i] <- diff_means(gg_data$observed, gg_data$treat)[3]

    }


## ------------------------------------------------------------------------
hist(dm, col="blue", main="Histogram of Difference of Means \n for GGdata")


## ------------------------------------------------------------------------

pop1 <- rnorm(50, 0, 5) # no outliers
pop2 <- c(rnorm(48, 0, 5), 8, 11) # two outliers


## ------------------------------------------------------------------------
apply(cbind(pop1, pop2), 2, FUN=mean)

## ------------------------------------------------------------------------

sample_means <- matrix(NA, 10000, 4)

for (i in 1:10000){
  sample_means[i,] <- c(mean(sample(pop1, 25, replace=T)),
                        mean(sample(pop1, 25, replace=F)),
                        mean(sample(pop2, 25, replace=T)), 
                        mean(sample(pop2, 25, replace=F)))
}

head(sample_means)


## ---- echo = FALSE, fig.height = 4---------------------------------------
sample_means <- as.data.frame(sample_means)
names(sample_means) <- c("pop1_with", "pop1_without",
                         "pop2_with", "pop2_without")

par(mfrow=c(1,2))
# plots for population 1
plot(density(sample_means$pop1_without), lwd=3, col="blue",
     xlim=c(-5, 5),
     main="Distribution of sample means \n population 1 (no outliers)")
lines(density(sample_means$pop1_with), lwd=3, col="red")
# plots for population 2
plot(density(sample_means$pop2_without), lwd=3, col="blue",
     xlim=c(-5, 5),
     main="Distribution of sample means \n population 2 (with outliers)")
lines(density(sample_means$pop2_with), lwd=3, col="red")


## ------------------------------------------------------------------------

apply(sample_means, 2, FUN=mean)
apply(sample_means, 2, FUN=var)


## ---- echo = FALSE-------------------------------------------------------

# loading the data - Dunning & Harrison (2010)

link <- "https://raw.githubusercontent.com/unc421/231b/master/Section%202/cross_cutting_apsr_replicationdata.csv"

cc_data <- read.csv(link)

# treat_assign takes on a value 1 through 6 and denotes the treatment condition to 
# which the respondent was assigned, as follows:
# 1 -- Same ethnicity, joking cousin
# 2 -- Same ethnicity, not joking cousin
# 3 -- Different ethnicity, joking cousin
# 4 -- Different ethnicity, not joking cousin
# 5 -- No last name given for candidate
# 6 -- Candidate and subject have same last name (and thus ethnicity)



## ---- echo = FALSE-------------------------------------------------------

box <- cc_data$vote_prefer[cc_data$treat_assign==1]
box <- as.data.frame(box)

m <- ggplot(box, aes(x=box))
m + geom_histogram(aes(y = ..density..), alpha=.25, binwidth=1) + 
  # We will overlay a blue line showing the normal distribution with mean equal to the mean
  # of the box and sd equal to the sd of the box
  stat_function(fun=dnorm, args=list(mean=mean(box$box), sd=sd(box$box)), 
                col="blue", size=.8) +
  # and a red line showing the mean of the box
  geom_vline(xintercept = mean(box$box), col="red", size=1.25) +
  theme_bw() + 
  labs(title="",  x="Respondent wants to vote for candidate (1-7 scale)",
       y="Density")


## ------------------------------------------------------------------------

# We first need to define the number of units we will draw from the box 
# (with replacement). 
# For the first example, let's take N=5
N <- 5

# If we wanted to do step (2) once, we would sample from the box N times with replacement
boot_sample <- sample(box$box, N, replace=T) 
# And then take the statistic of interest for this bootstrap sample, here the mean. 
mean(boot_sample)


## ------------------------------------------------------------------------
boot_reps <- 10000 # number of bootstrap replicates we will repeat step (2) for
boot_mean <- NA # placeholder vector for the results

for (i in 1:boot_reps){
  boot_sample <- sample(box$box, N, replace=T) 
  boot_mean[i] <- mean(boot_sample)
}


## ---- echo=FALSE, fig.height=3-------------------------------------------
# Now we plot the results
m <- ggplot(as.data.frame(boot_mean), aes(x=boot_mean))
# First we plot a histogram with the results
m + geom_histogram(aes(y = ..density..), alpha=.5, binwidth=.1) + 
  # and overlay a line with the density of a normal distribution with mean equal to the 
  # mean of the bootstrap means and sd equal to the sd of the bootstrap means.
  stat_function(fun=dnorm, 
                args=list(mean=mean(boot_mean), sd=sd(boot_mean)), 
                col="blue", size=1) +
  # and we add a vertical line for the mean of the box
  geom_vline(xintercept=mean(box$box), col="red", size=1) + theme_bw()


## ------------------------------------------------------------------------
bootstrap_mean <- function(data, replicates=10000, N, bin, title="", 
                           xlab="", ylab="", xmax, xmin){
  
  boot_mean <- NA #plaaceholder vector
  for (i in 1:replicates){
    # we sample N units from the box with replacement
    boot_sample <- sample(data, N, replace=T)
    # and save their mean
    boot_mean[i] <- mean(boot_sample)
  }
  
  m <- ggplot(as.data.frame(boot_mean), aes(x=boot_mean))
  # First we plot a histogram with the results
  m + geom_histogram(aes(y = ..density..), alpha=.5, binwidth=bin) + 
    # and overlay a line with the density of a normal distribution with mean equal to the mean
    # of the bootstrap means and sd equal to the sd of the bootstrap means.
    stat_function(fun=dnorm, 
                  args=list(mean=mean(boot_mean), sd=sd(boot_mean)), col="blue", size=1) +
    # and we add a vertical line for the mean of the box
    geom_vline(xintercept=mean(box$box), col="red", size=1) +
    # limits for x axis
    scale_x_continuous(limits = c(xmin, xmax)) +
    # and labels
    labs(title=title,  x=xlab, y=ylab) + theme_bw()
}


## ---- fig.height=4-------------------------------------------------------
bootstrap_mean(data=box$box, replicates=10000, N=5, bin=.035, 
               # title="N=5", 
               xmin=1, xmax=7,
               xlab="Mean - Respondent wants to vote for candidate (1-7 scale)", 
               ylab="Density")

## ---- fig.height=4-------------------------------------------------------
bootstrap_mean(data=box$box, replicates=10000, N=25, bin=.035, 
               # title="N=25", 
               xmin=1, xmax=7,
               xlab="Mean - Respondent wants to vote for candidate (1-7 scale)", 
               ylab="Density")

## ---- fig.height=4-------------------------------------------------------
bootstrap_mean(data=box$box, replicates=10000, N=100, bin=.035,  
               # title="N=100",
               xmin=1, xmax=7,
               xlab="Mean - Respondent wants to vote for candidate (1-7 scale)", 
               ylab="Density")

## ------------------------------------------------------------------------

#Gerber and Green (2012); Chattopadhyay and Duflo (2004)

# the data
gg_data <- as.data.frame(cbind(c(10,15,20,20,10,15,15), 
                               c(15,15,30,15,20,15,30)))
names(gg_data) <- c("Y_i0", "Y_i1")

## ------------------------------------------------------------------------
# generating empty dataframe to put the results
ate <- as.data.frame(matrix(NA, 10000, 2))
names(ate) <- c("estimated_ate", "estimated_se_ate")

# sampling
for (i in 1:10000){
  
  # generating treatment vector for this replicate
  gg_data$treat <- 0
  gg_data$treat[sample(1:7, 2, replace=F)]  <- 1
  
  treat_mean <- mean(gg_data$Y_i1[gg_data$treat==1])
  treat_var <- var(gg_data$Y_i1[gg_data$treat==1])
  
  control_mean <- mean(gg_data$Y_i0[gg_data$treat==0])
  control_var <- var(gg_data$Y_i0[gg_data$treat==0])
  
  ate[i,1] <- treat_mean - control_mean
  ate[i,2] <- sqrt(treat_var/2 + control_var/5) 
}


## ---- echo=FALSE---------------------------------------------------------

m <- ggplot(ate, aes(x=estimated_ate))
m + 
  geom_histogram(aes(y = 100 * (..count..)/sum(..count..)), binwidth=.5, alpha=.5) + 
  # geom_histogram(aes(y = ..density..)) +
  geom_vline(xintercept=mean(ate$estimated_ate), col="red", size=1.25) +
  theme_bw() +
  xlab("Estimated ATE") +
  ylab("Percent")


## ------------------------------------------------------------------------

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
estimated_se # Why is this number different from 7.75 (the number in the lecture slides)?
# rounding error!


## ------------------------------------------------------------------------

# converting to standard units
t_stat <- (ATE - 0) / estimated_se
t_stat

# To be able to get the right Student t Distribution, we need to calculate
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
# One tailed p-value
pt(t_stat, df=df, ncp=0, lower.tail=F)

# Two tailed p-value
pt(-t_stat, df=df, ncp=0, lower.tail=T) + pt(t_stat, df=1.12, ncp=0, lower.tail=F)


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


## ---- echo=FALSE, fig.height=4, fig.cap="Distribution of randomization ATEs"----

m <- ggplot(as.data.frame(rand_ate), aes(x=rand_ate))
m + 
  geom_histogram(aes(y = ..density..), binwidth=.5) +
  geom_vline(xintercept=ATE, col="red", size=1) +
  theme_bw() +
  xlab("Randomization ATEs") +
  ylab("Density")


## ------------------------------------------------------------------------

# One tailed
sum(rand_ate>=ATE)/length(rand_ate)

# Two tailed
sum(abs(rand_ate)>=ATE)/length(rand_ate)



