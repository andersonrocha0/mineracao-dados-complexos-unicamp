rm(list = ls())

setwd('~/DataScience/Unicamp/INF-0615/2018-04-14')

trainData <- read.csv("breastCancer_train.data", header = T, sep = ",")
testData <- read.csv("breastCancer_test.data", header = T, sep = ",")

# Análises
summary(trainData)
nrow(trainData)
#cor(trainData[c(c(2:6), 8:11)], use = "na.or.complete")

# Remoção do id
trainData[, "id"] <- NULL
testData[, "id"] <- NULL

# Conversão para númerico do campo bare.nuclei
trainData[, 6] <- as.numeric((trainData[, 6]))
testData[, 6] <- as.numeric((testData[, 6]))

trainData = trainData[trainData$bare.nuclei != 11,]
testData = testData[testData$bare.nuclei != 11,]

formula = as.formula("class ~ clump.thickness + unif.cell.size + unif.cell.shape + 
                marginal.adhesion + epithelial.cell.size + bare.nuclei +       
                bland.chromatin + normal.nucleoli + mitoses")
#### error!!! class must be a 0 <= class <= 1
model = glm(formula, trainData, family=binomial(link="logit"))
print(trainData$class)

trainData[trainData$class == 2, "class"] = 0
trainData[trainData$class == 4, "class"] = 1

testData[testData$class == 2, "class"] = 0
testData[testData$class == 4, "class"] = 1

print(trainData$class)

model = glm(formula, trainData, family=binomial(link="logit"))
print(trainData$class)

testPred <- predict(model, testData[,1:9], type = "response")

testPred[testPred >= 0.5] <- 1
testPred[testPred < 0.5] <- 0

## Para verificar os acertos
table(testData$class == testPred)

## Pode-se usar também matriz de confusão
cm <- as.matrix(table(Actual = testData$class, Predicted = testPred))

## Acurácia - quanto eu acerto de forma geral . Soma da diagonal dividido pelo soma da matriz inteira
sum(diag(cm)) / sum(cm)

## Acurácia normalizada
tnr <- cm[1,1] / sum(cm[1, ]) ## Fração do primeiro item - true negative rate
tpr <- cm[2,2] / sum(cm[2, ]) ## Fração do segundo item - true positive rate
mean(tpr, tnr) ## Média das duas