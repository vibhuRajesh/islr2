require(ISLR)
require(MASS)



#Split data into training and test
train <- Smarket %>% filter(Year < 2005)

test <- Smarket %>% filter(Year >= 2005)


#Linear Discriminant Analysis

#first fit of direction ~ lag1-2
lda.fit1 <- lda(data = train, Direction ~ Lag1 + Lag2)

#Details of LDA
lda.fit1

plot(lda.fit1)


#Predict funcion for test data
lda.pred1 <- predict(lda.fit1, newdata = test)

#colnames of LDA function
names(lda.pred1)

#Convert lda prediction  to data frame
lda.pred1 <- as.data.frame(lda.pred1)

#Confusion matrix
  #Use predicted class:(lda1.pred$class) <- posterior = up or down
  #compare to actual: (test$Direction)
tab1 <- table(lda.pred1$class, test$Direction)
tab1

#Accuracy tester
  #Use 'class' attribute
  #Compare to actual (test$Direction)
mean(lda.pred1$class == test$Direction)