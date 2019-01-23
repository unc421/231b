
##Create 231b groups for problem sets

library(foreign)
library(RCurl)
library(httr)


link <- "https://raw.githubusercontent.com/unc421/231b/master/Group%20assignment/names.csv"

names <- read.csv(link)



set.seed(64923)

##Create a vector of group numbers 
##We want 5 groups so there should be five 1's, five 2's, etc.
##We have a couple of ways to do this

#With the rep function
group <- c(rep(1, 5), rep(2, 5), rep(3,5), rep(4,5), rep(5, 5))

#With a for loop

#We use a matrix because our for loop will output 4 numbers in each iteration (instead of just one)

groups <- matrix(NA, 5, 4)

for ( i in 1:5){
  
  groups[i,] <- rep(i, 4)
  
}

#Using sapply

groups <- sapply(seq(1:5), function(x) rep(x, 4))


##Now we want to randomly sample 20 times without replacement from the groups object. We want a random ordering of group numbers. 

assignment <- sample(groups, 20, replace = FALSE)

##Create a dataframe with names and group assignment
final <- cbind.data.frame(names$name, assignment)

#Name columns in the data frame
names(final) <- c("name", "group")

#Final group assignment
final[order(final$group),]


      