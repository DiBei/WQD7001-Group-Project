---
title: "Model"
author: "WQD7001 Group 3"
date: "2022/1/16"
output: html_document
---

```{r}
library(caret)
library(imbalance)
library(randomForest)

read.csv("new_diabetes.csv")

mydata <- read.csv("new_diabetes.csv")
# Partition data 70/30 using any method you feel comfortable with
sample <- createDataPartition(y = mydata$DiabetesPedigreeFunction, p = 0.7, list = FALSE)
train <- mydata[sample, ]
test <- mydata[-sample, ]

# Check for cross validation if the model allows for it
train_control <- trainControl(method="cv", number=5)

# Model training
# 1. Random Forest
randomForest <- train(Outcome ~ ., data = train, method = "rf", trControl = train_control)
randomForest

# 2. K-Nearest Neighbor
knn <- train(Outcome~., data = train, method = "knn", trControl = train_control)
knn

# 3. Logistic Regression
logisticRegression <- train(Outcome~., data = train, method = "glm", 
                            trControl = train_control)
logisticRegression

test$prediction <- predict(randomForest, newdata = test)

# Model testing
test$Outcome=as.factor(test$Outcome)
test.rf<- randomForest(Outcome~.,data=test, importance=TRUE,proximity=TRUE, type=classification, ntree=300)
print(test.rf)

importance(test.rf,type=1)
# We can see the most important features are Glucose, BMI and age.
```