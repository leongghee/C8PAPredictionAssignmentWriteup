library(caret)
library(rpart)
library(rattle)
library(randomForest)

set.seed(126)

TrainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
TestURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#training <- read.csv(url(TrainURL),na.strings = c("#DIV/0!","NA",""))
#testing <- read.csv(url(TestURL),na.strings = c("#DIV/0!","NA",""))


training <- read.csv("pml-training.csv",na.strings = c("#DIV/0!","NA",""))
testing <- read.csv("pml-testing.csv",na.strings = c("#DIV/0!","NA",""))

training <-training[,colSums(is.na(training))==0]
testing <-testing[,colSums(is.na(testing))==0]

training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]

inTrain <- createDataPartition(training$classe, p = 0.60, list = FALSE)
TrainSet = training[ inTrain,]
TestSet = training[-inTrain,]

dim(TrainSet)
dim(TestSet)

modelTree <- rpart(classe ~ .,data = TrainSet, method = "class")
fancyRpartPlot(modelTree)
predictTree <- predict(modelTree, newdata = TestSet, type = "class")
confusionMatrix(predictTree,TestSet$classe)

modelForest <- randomForest(classe ~ ., data = TrainSet)
predictForest <- predict(modelForest, newdata = TestSet, type = "class")
confusionMatrix(predictForest,TestSet$classe)


predictTest <- predict(modelForest, newdata = testing)
predictTest

knit("C8PA.Rmd")