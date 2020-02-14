# import libraries
library(caret)
library(mlbench)
library(tidyverse)

# load data
completeResponses <- read.csv('CompleteResponses.csv')
incompleteResponses <- read.csv('SurveyIncomplete.csv')

# understand data structure
summary(completeResponses)
str(completeResponses)
summary(incompleteResponses)
str(incompleteResponses)

# change brand preference to factor
completeResponses$brand <- as.factor(completeResponses$brand)
incompleteResponses$brand <- as.factor(incompleteResponses$brand)

# set seed
set.seed(123)

# create training and testing partitions
inTrain <- createDataPartition(completeResponses$brand, p = .75, list = FALSE)
training <- completeResponses[inTrain,]
testing <- completeResponses[-inTrain,]

# set fit control
fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 1)

# random forest
rfGrid <- expand.grid(mtry = c(3))
rfFit <- train(brand~., data = training, method = 'rf', trControl = fitControl, tuneGrid = rfGrid)

# c5.0
c5 <- train(brand~., data = training, method = 'C5.0', trControl = fitControl, tuneLength = 2)

# predict brand preference of incomplete surveys
predictionsC5 <- predict(c5, incompleteResponses)
predictionsRF <- predict(rfFit, incompleteResponses)

# summarize predictions
summary(predictionsC5)
summary(predictionsRF)