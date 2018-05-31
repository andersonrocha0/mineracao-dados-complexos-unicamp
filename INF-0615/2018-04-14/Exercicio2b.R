rm(list = ls())

setwd('~/DataScience/Unicamp/INF-0615/2018-04-14')

data <- read.csv("abalone.data", header = T, sep = ",")

data[data$rings < 15, "rings"] <-  0
data[data$rings >= 15, "rings"] <-  1

data$male <- ifelse(data$sex == "M", 1, 0)
data$female <- ifelse(data$sex == "F", 1, 0)
data$other <- ifelse(data$sex == "I", 1, 0)

## TODO 

set.seed(42)

randomTrainIndexes = sample(1:nrow(data), size=0.8*nrow(data))
trainData = data[randomTrainIndexes,]
valData = data[-randomTrainIndexes,]

formula = as.formula("rings ~ length + diameter + height + whole_weight 
                     + shucked_weight + viscera_weight + shell_weight
                     + male + female + other")

model = glm(formula, trainData, family=binomial(link="logit"))
print(trainData$rings)

testPred <- predict(model, valData[,2:11], type = "response")

testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0

# Matriz de confusão
cm <- as.matrix(table(Actual = valData$rings, Predicted = testPred))
print(cm)

#ACC = (TP + TN) / total
ACC = (cm[1,1] + cm[2,2]) / sum(cm)
print(ACC)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR = cm[2,2] / (cm[2,2] + cm[2,1])
print(TPR)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR = cm[1,1] / (cm[1,1] + cm[1,2])
print(TNR)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm = mean(TPR, TNR)
print(ACCNorm)
