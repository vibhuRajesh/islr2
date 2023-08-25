require(boot)
require(ISLR)
library(tidyverse)

#Cross Validation Lab

#Leave One Out Cross Validation
  
  #n observations ∴ k = n
  

  #Load data set
  #graph <- ggplot(data = Auto, aes(x = horsepower, y = mpg)) +
   #       geom_point(col = 'red')
  #graph
  

  glm.fit <- glm(mpg ~ horsepower, data = Auto)
  summary(glm.fit)
  
  ?cv.glm
  
  #CV R function
  cvglm <- cv.glm(data = Auto, glmfit = glm.fit)
  
  # K & Delta Value: 
    #1: loocv estimate of error
    #2: bias corrected estimate of error 
  
  #K-value: double check k == nrow(Auto) == 392
  cvglm$delta
  cvglm$K
  
  
  
  #Write function that solves using formula that takes into account 
  #leverage of element: 1- h -> diagonals of matrix h are 
  

  ?lm.influence
  
  loocv <- function(fit){
    
    #Numerator: residual sum squares (actual dataset - auto dataset)
    rss <- residuals(fit) ^ 2
    
    #Denominator: diag of hat matrix (leverage: influence point has on fit)
    hat_diag <- (lm.influence(fit)$hat)
    
    #average of ratios    
    mean(rss / (1 - hat_diag) ^ 2)
    
  
  }
  
  loocv(glm.fit)
  
  
  
  #Test loocv function with different degree polynomials:
  
    #Initialize 'empty' vector of size 5 for collecting error
        #repeat function --> 0 repeated 5 times
    cv.error <- rep(0, 10)
    
    #Degree 1-10
    deg <- 1:10
    
    
    #(glm of (polynomial regression of horsepower with (degree d)))
    for(d in deg){
      glm.fit2 <- glm(data = Auto, mpg ~ poly(horsepower, degree = d))
      cv.error[d]  <- loocv(glm.fit2)      
    }
    
    cv.error
    
    plot(cv.error ~ deg, col = 'red', type = 'b', ylim = c(18, 25))
    

    
  #10 fold CV testing multiple degree polynomials
    
    #vector for storing errors of each degree
    cv.error2 <- rep(0, 10)
    
    degree <- 1:10
    
    for(e in degree){
      
      #glm model
      glm.fit3 <- glm(mpg ~ poly(horsepower, degree = e), data = Auto)
      
      #Cross Validation function cv.glm with K = 10
      cv.glm3 <- cv.glm(data = Auto, glm.fit3, K = 10)

      err <- cv.glm3$delta[1]
      
      #add to error vector
      
      cv.error2[e] <- err
      
    }
    
    #check vector of CV errors for each degree of polynomial
    cv.error2
    
    plot(cv.error2 ~ deg, type = 'b', col = 'red')
  
    
    
  
  
  