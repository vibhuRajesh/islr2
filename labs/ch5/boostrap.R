library(learnr)
require(ISLR)
require(tidyverse)
require(boot)



#Example used: 
  
#Two investments with returns X and Y
  
# (alpha) into X ----->  (1-alpha) into Y
  
#Minimize risk (variance) of returns -> (alpha * X) + ((1-alpha) * Y)


#write function to solve for optimized value of alpha using minimized function 
  #formula from example:
  
alpha <- function(x, y){
  
  #Variance and Covariances
  vx <- var(x)
  vy <- var(y)
  cov_xy <- cov(x, y)
  
  #numerator and denominator for calculating alpha
  num <- vy - cov_xy
  denom <-(vx + vy - 2 * cov_xy)
  
  #alpha
  a <- num / denom
  a
  
}


#Dataset: Portfolio with assets: X and Y:


?Portfolio
#examine
head(Portfolio)

X <- Portfolio$X
Y <- Portfolio$Y

alpha(X, Y)


#Function that passes in index and returns corresponding alpha value 
  #Calculates alpha for 

alpha.fn <- function(df, index){
  
  with(df[index,], alpha(X, Y))
  
}


alpha.fn(Portfolio, 1:100)


#Bootstrap 

#Random numbers are accessible again and reproducible
set.seed(1)




#Bootstrap function

#second parameter (statistic):
  #function that returns the value being simulated (alpha) for all 
  #random n length samples 

boot.out <- boot(Portfolio, alpha.fn, R = 1000)
boot.out


plot(boot.out)







