library(tidyverse)
library(ISLR)
library(class)

#param: training data
#param: test data
#param: training data response (to compare to)
#param: k -> k nearest elements
?knn

#Split into train and test
train <- Smarket %>% filter(Year < 2005)

test <- Smarket %>% filter(Year == 2005)



#Include Lag1 and Lag2 in one df for training and test data

trainLag <- train %>% dplyr::select(Lag1, Lag2)

testLag <- test %>% dplyr::select(Lag1, Lag2)


#KNN Model
  #trainLag <- lag1, lag2data (2001-2004)   (X)
  #train$Direction <- Results of trainLag (Y)
  #testLag <- test lag data (2005) ()

knn.fit <- knn(trainLag, testLag, cl = train$Direction, k = 1)

#Make confusion matrix
  #test$Direction = testing data actual results

tab1 <- table(knn.fit, test$Direction)
tab1

#Accuracy at k = 1
mean(knn.fit == test$Direction)






mat <- {}



#testing out different values for k and plotting
for (x in 1:30){
  
  knn.fit2 <- knn(trainLag, testLag, cl = train$Direction, k = x)
  
  tab2 <- table(knn.fit2, test$Direction)
  
  v <- mean(knn.fit2 == test$Direction)
  
  mat[x] <- v
  

  
}

#plot of accuracy ~ k value
plot(mat, col = 'red', pch = 16, xlab = "k-value", ylab = "Accuracy")



















