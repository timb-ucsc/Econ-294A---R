## Tim Boudreau
## Econ 294A - R Lab notes
## March 4 2016
## Matt Baumer

library(caret)
library(e1071)

train <- read.csv("/Users/Tim/Desktop/UCSC/Winter 2016/Econ 294A - R/Econ Lab Git/assignments/train.csv")

train[,1] <- as.integer(train[,1])
set.seed(1212)
inPartition <- createDataPartition(train[,1], p = .5, list = FALSE)

test <- train[inPartition,]
train <- train[-inPartition,]

colVariances <- apply(X = train, FUN = var, MARGIN = 2)
badFeatures <- which(colVariances == 0)

train <- train[,-badFeatures]
train[,1] <- as.factor(train[,1])
test <- test[,-badFeatures]
test[,1] <- as.factor(test[,1])

## Running this takes FOREVER - DO NOT DO
#control <- rfeControl(functions=rfFuncs, method = "cv", number = 10)
#results <- rfe(train[,2:573], train[,1], sizes = c(1:50), rfeControl=control)
#top50 <- predictors(results)[1:50]
#top100 <- predictors(results)[1:100]
#top150 <- predictors(results)[1:150]


