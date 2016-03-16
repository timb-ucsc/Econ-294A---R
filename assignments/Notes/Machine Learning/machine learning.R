## Tim Boudreau
## machine learning practice
## March 4 2016


##############################
### Get Data and Summarize ###
##############################


# download data
train <- read.csv("/Users/Tim/Desktop/UCSC/Winter 2016/Econ 294A - R/Econ Lab Git/assignments/Machine Learning/train.csv", stringsAsFactors = FALSE)
test <- read.csv("/Users/Tim/Desktop/UCSC/Winter 2016/Econ 294A - R/Econ Lab Git/assignments/Machine Learning/test.csv", stringsAsFactors = FALSE)

# summarize data
str(train)

table(train$Survived)
prop.table(table(train$Survived))

summary(train$Sex)
prop.table(table(train$Sex, train$Survived)) # summarizes from total
prop.table(table(train$Sex, train$Survived), 1) # summarizes per group

# generate survived column in test data, predict who survives
test$Survived <- rep(0,length(test))

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1


######################
### Decision Trees ###
######################

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# regression to fit the data, then display the tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

# better display
fancyRpartPlot(fit)

# predict using the fit
Prediction <- predict(fit, test, type = "class")

# better prediction, with changed control parameters
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)



