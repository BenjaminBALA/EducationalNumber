install.packages("tidyverse")
library(tidyverse)
library(modelr)
install.packages("caret")
library(caret)
library(MASS)
library(car)


adult <- read.csv("C:/Users/benan/Desktop/ExmPA-Actex/adult.csv")

set.seed(123)  

split1<- sample(c(rep(0, 0.8 * nrow(adult)), rep(1, 0.2 * nrow(adult))))
#This is the code to run a 80% train test split

train <- adult[split1 == 0, ] 
#Splits the training data

test <- adult[split1== 1, ]   
#Splits the test data

model1 <- lm(educational.num ~ ., data=train) 
#This is a linear model with all the variables included

model2 <- lm(educational.num ~ age + workclass, data=train)
#This is a linear model with just age and working class included

model3 <- lm(educational.num ~ age + workclass + marital.status, data=train)
#This is a linear model with age, working class, and marital.status included

model4 <- lm(educational.num ~ age + workclass + marital.status + occupation, data=train)
#This is a linear model with age, working class, marital status, and occupation

AIC(model1) #AIC of model 1
AIC(model2) #AIC of model 2
AIC(model3) #AIC of model 3
AIC(model4) #AIC of model 4

stepAIC(model1)
#This function runs the AIC stepwise selection of the model and returns the best model for AIC predictive performance
#The best model to fit the training data is the following model

model5 <- lm(educational.num ~ fnlwgt + education + race, data = train)
#This turns out to be the best model for reducing the AIC
#However, this is a perfect fit to the training data and is a sign of overfitting

model6 <- lm(educational.num ~ age + workclass + marital.status + occupation + relationship, data=train)
AIC(model6)
#This model is the lowest possible AIC for any model after using stepwise selection


pred <- predict(model6, newdata = test)
summary(model6)

error <- test$educational.num - pred
RMSE <- sqrt(mean(error^2))
RMSE
