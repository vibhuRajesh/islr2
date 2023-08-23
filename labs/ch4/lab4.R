library(tidyverse)
#Load ISLR package library with datasets
require(ISLR)



#Scatterplot matrices of all variables
data1 <- pairs(Smarket, col = Smarket$Direction)
data1

#Summary of data
summary(Smarket)

#Correlation matrix
  #only 1-8 since [9] is chr type not num
cor(Smarket[1:8])

attach(Smarket)
viz <- plot(Volume)

#glm to fit direction ~ lag1-5
glm.fit <- glm(data = Smarket, Direction ~ Lag1+  Lag2 + Lag3
               + Lag4 + Lag5 + Volume, family = binomial)

#Summary of model
summary(glm.fit)


#Summary of coefficients of Lag1 - Lag5
coef(glm.fit)


#Predict function to get probability at particular point

  # Type = "Response" : 
glm.prob <- predict(glm.fit, type = "response")


#Classify as 'UP'  if >0.50; 'DOWN' otherwise
glm.class <- ifelse(glm.prob > 0.5, "Up", "Down")

#Frequency Table comparing glm classifications and actual Direction in table
newMat <- table(glm.class, Direction)
newMat

#Proportion of accuracies using mean() function
accu <- mean(glm.class == Smarket$Direction)
accu


#Divide given data => training and test
train <- Smarket %>% filter(Year < 2005)

test <- Smarket %>% filter(Year >= 2005)

#Verify 2005 isn't in end of list
tail(train)
head(test)

#New model: build - > Use predict to evaluate - > Classify

#Build
glm.fit.2 <- glm(data = train, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(glm.fit.2)

#Predict for test data
glm.prob.2 <- predict(glm.fit.2, newdata = test, type = "response")

#Classify by threshold
glm.class.2 <- ifelse(glm.prob.2 > 0.5, "Up", "Down")


#Confusion matrix
tab2 <- table(glm.class.2, test$Direction)
tab2
?table



#proportion of accuracy
accu2 <- mean(glm.class.2 == test$Direction)

accu2



#Try smaller model to decrease overfit : less features, use just lag1 and lag2

#build model
small.glm <- glm(data = train, Direction ~ Lag1 + Lag2, family = binomial)

#Predict with mode
small.prob <- predict(small.glm, test, type = "response")

#Classify with ifelse
small.class <- ifelse(small.prob > 0.50, "Up", "Down")


#Table comparing model prediction and actual
tab.small <- table(small.class, test$Direction)
tab.small

#Calculate proportion of accuracy

temp <- mean(small.class == test$Direction)
temp

#Summary of smaller model
summary(small.glm)