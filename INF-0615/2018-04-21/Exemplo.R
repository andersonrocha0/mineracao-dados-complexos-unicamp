setwd("/home/anderson/DataScience/Unicamp/INF-0615/2018-04-21/")

rm(list = ls())

# Carregando os dados
trainData <- read.csv(file = "kyphosis_train.data", header = T, sep = ",")

## Para testar, trocar o arquivo para test.data
valData <- read.csv(file = "kyphosis_val.data", header = T, sep = ",")
#valData <- read.csv(file = "wineQuality_test.data", header = T, sep = ",")

# Inspecionando os dados
nrow(trainData)
# Total de 3898
summary(trainData)
cor(trainData)
table(trainData$Kyphosis)

library(rpart)

#params <- list(split = "information")
params <- list(split = "gini")
treeModel <- rpart(data = trainData, formula = Kyphosis ~ Age + Number + Start, method = "class", parms = params)

# Usar quando houver over fitting
ptree <- prune(treeModel, cp = 0.73)


plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)

printcp(treeModel)

prediction = predict(treeModel, valData)
prediction = as.numeric(prediction[,"present"] >= 0.5)
prediction[prediction==0] = "absent"
prediction[prediction==1] = "present"

CM = as.matrix(table(Actual = valData$Kyphosis, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))

ACCNorm

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(treeModel)

#install.packages("randomForest")

library(randomForest)

rfModel <- randomForest(formula = Kyphosis ~ Age + Number + Start, data = trainData, ntree = 10)

predictionRf = predict(rfModel, valData)


rfCM = as.matrix(table(Actual = valData$Kyphosis, Predicted = predictionRf))
TPRrf = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
TNRrf = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
ACCNormrf = mean(c(TPRrf, TNRrf))

ACCNormrf


## Iris

plot(iris)
set.seed(42)

data <- iris

idx <- sample(1:150)

trainData <- data[idx[1:120],]
valData <- data[idx[121:150],]

table(trainData$Species)
table(valData$Species)


params <- list(split = "gini")
treeModel <- rpart(data = trainData, formula = Species ~ ., method = "class", parms = params)


plot(treeModel, uniform=TRUE)
text(treeModel, use.n=TRUE, all=TRUE, cex=.8)
printcp(treeModel)
rpart.plot(treeModel)

prediction = predict(treeModel, valData, type = "class")

CM = as.matrix(table(Actual = valData$Species, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))

ACCNorm


# Random forest
rfModel <- randomForest(formula = Species ~ ., data = trainData, ntree = 50)

predictionRf = predict(rfModel, valData, type = "class")


rfCM = as.matrix(table(Actual = valData$Species, Predicted = predictionRf))
TPRrf = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
TNRrf = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
ACCNormrf = mean(c(TPRrf, TNRrf))

ACCNormrf
