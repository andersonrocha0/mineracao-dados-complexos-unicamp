########################################
# Trabalho - INF-0615          
# Nome(s): Anderson Rocha / Alexandre Guidin
########################################

setwd("/home/anderson/DataScience/Unicamp/INF-0615/Trabalho2/")

rm(list = ls())

# Carregando os dados
trainData <- read.csv(file = "wineQuality_train.data", header = T, sep = ",")

## Para testar, trocar o arquivo para test.data
validationData <- read.csv(file = "wineQuality_test.data", header = T, sep = ",")
#validationData <- read.csv(file = "wineQuality_test.data", header = T, sep = ",")

# Inspecionando os dados
nrow(trainData)
# Total de 3898
summary(trainData)
cor(trainData)
unique(trainData$quality)
# Verificando se há somente 1 e 0 na qualidade

# Normalizando
meanTrainFeatures <- apply(trainData[1:11], 2, mean)
stdTrainFeatures <- apply(trainData[1:11], 2, sd)

trainData[,1:11] = sweep(trainData[,1:11], 2, meanTrainFeatures, "-")
trainData[,1:11] = sweep(trainData[,1:11], 2, stdTrainFeatures, "/")

validationData[,1:11] = sweep(validationData[,1:11], 2, meanTrainFeatures, "-")
validationData[,1:11] = sweep(validationData[,1:11], 2, stdTrainFeatures, "/")


####### Treinando a regressão logística
formula = as.formula("quality ~ fixed.acidity + volatile.acidity + citric.acid + 
                     residual.sugar + chlorides + free.sulfur.dioxide + 
                     total.sulfur.dioxide + density + pH + sulphates + alcohol")

model = glm(formula, trainData, family=binomial(link="logit"))

validationPred = predict(model, validationData[,1:11], type="response")

# Convertendo para bom ou ruim (classe)
validationPred[validationPred >= 0.5] = 1
validationPred[validationPred < 0.5] = 0

# Matriz de confusão
cm = as.matrix(table(Actual = validationData$quality, Predicted = validationPred))
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
ACCNorm = mean(c(TPR, TNR))
print(ACCNorm)



# Tentativa 2 - Usando menos dimensões

####### Treinando a regressão logística
formula2 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol")

model2 = glm(formula2, trainData[, c(9, 8, 1, 2, 3, 11, 12)], family=binomial(link="logit"))

validationPred2 = predict(model2, validationData[, c(9, 8, 1, 2, 3, 11, 12)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred2[validationPred2 >= 0.5] = 1
validationPred2[validationPred2 < 0.5] = 0

# Matriz de confusão
cm2 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred2))
print(cm2)


#ACC = (TP + TN) / total
ACC2 = (cm2[1,1] + cm2[2,2]) / sum(cm2)
print(ACC2)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR2 = cm2[2,2] / (cm2[2,2] + cm2[2,1])
print(TPR2)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR2 = cm2[1,1] / (cm2[1,1] + cm2[1,2])
print(TNR2)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm2 = mean(c(TPR2, TNR2))
print(ACCNorm2)

# Não mudou muita coisa, ficou um pouco pior.


# Tentativa 3 - Combinando duas dimensões

trainData$fixed.acidity.vs.alcohol <- trainData$fixed.acidity * trainData$alcohol
validationData$fixed.acidity.vs.alcohol <- validationData$fixed.acidity * validationData$alcohol

####### Treinando a regressão logística
formula3 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol + fixed.acidity.vs.alcohol")

model3 = glm(formula3, trainData[, c(1, 2, 3, 8, 9, 11, 12, 13)], family=binomial(link="logit"))

validationPred3 = predict(model3, validationData[, c(1, 2, 3, 8, 9, 11, 12, 13)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred3[validationPred3 >= 0.5] = 1
validationPred3[validationPred3 < 0.5] = 0

# Matriz de confusão
cm3 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred3))
print(cm3)


#ACC = (TP + TN) / total
ACC3 = (cm3[1,1] + cm3[2,2]) / sum(cm3)
print(ACC3)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR3 = cm3[2,2] / (cm3[2,2] + cm3[2,1])
print(TPR3)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR3 = cm3[1,1] / (cm3[1,1] + cm3[1,2])
print(TNR3)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm3 = mean(c(TPR3, TNR3))
print(ACCNorm3)

# :( nada mudou ainda.

# Tentativa 4 - Balanceando dados e usando as mesmas dimensões da segunda tentativa

require(ggplot2)
require(reshape2)

a <- melt(trainData[, c(1, 2, 3, 8, 9, 11, 12, 13)])


# Plotando gráfico tipo dispersão
#ggplot(data = a, aes(x=variable, y=value)) + 
#  geom_jitter(aes(colour=variable)) +
#  theme(axis.text.x = element_blank())

# Verificando outliers em densidade
trainData[trainData["density"] > 5, ]

# Removendo outliers
trainData <- trainData[!trainData["density"] > 5, ]

# Verificando balanceamentos dos dados
table(trainData$quality)

# Oversampling

vinhosBons <- trainData[trainData$quality == 1, ]

trainDataOverSampling <- rbind(trainData, vinhosBons)
trainDataOverSampling <- rbind(trainDataOverSampling, vinhosBons)
trainDataOverSampling <- rbind(trainDataOverSampling, vinhosBons)

table(trainDataOverSampling$quality)

trainData <- trainDataOverSampling

# plotando gráfico novamente

a <- melt(trainData[, c(1, 2, 3, 8, 9, 11, 12, 13)])


# Plotando gráfico tipo dispersão
#ggplot(data = a, aes(x=variable, y=value)) + 
#  geom_jitter(aes(colour=variable)) +
#  theme(axis.text.x = element_blank())


####### Treinando a regressão logística
formula4 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol")

model4 = glm(formula4, trainData[, c(9, 8, 1, 2, 3, 11, 12)], family=binomial(link="logit"))

validationPred4 = predict(model4, validationData[, c(9, 8, 1, 2, 3, 11, 12)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred4[validationPred4 >= 0.5] = 1
validationPred4[validationPred4 < 0.5] = 0

# Matriz de confusão
cm4 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred4))
print(cm4)


#ACC = (TP + TN) / total
ACC4 = (cm4[1,1] + cm4[2,2]) / sum(cm4)
print(ACC4)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR4 = cm4[2,2] / (cm4[2,2] + cm4[2,1])
print(TPR4)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR4 = cm4[1,1] / (cm4[1,1] + cm4[1,2])
print(TNR4)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm4 = mean(c(TPR4, TNR4))
print(ACCNorm4)


# Tentativa 5 - Aplicando log aos valores

trainDataLogs <- trainData[, c(9, 8, 1, 2, 3, 11, 12)]
trainDataLogs[,1:6] <- log(abs(trainDataLogs[, 1:6]))


####### Treinando a regressão logística
formula5 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol")

model5 = glm(formula5, trainDataLogs[, 1:7], family=binomial(link="logit"))

validationPred5 = predict(model5, validationData[, c(9, 8, 1, 2, 3, 11, 12)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred5[validationPred5 >= 0.5] = 1
validationPred5[validationPred5 < 0.5] = 0

# Matriz de confusão
cm5 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred5))
print(cm5)


#ACC = (TP + TN) / total
ACC5 = (cm5[1,1] + cm5[2,2]) / sum(cm5)
print(ACC5)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR5 = cm5[2,2] / (cm5[2,2] + cm5[2,1])
print(TPR5)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR5 = cm5[1,1] / (cm5[1,1] + cm5[1,2])
print(TNR5)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm5 = mean(c(TPR5, TNR5))
print(ACCNorm5)


# Tentativa 6 - Quadrático

quadratico <- function(x) {
  x ^ 2 + x
}

quadratico <- Vectorize(quadratico)

trainDataQuad <- trainData[, c(9, 8, 1, 2, 3, 11, 12)]
trainDataQuad[,1:6] <- quadratico(trainDataQuad[, 1:6])


####### Treinando a regressão logística
formula6 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol")

model6 = glm(formula6, trainDataQuad[, 1:7], family=binomial(link="logit"))

validationPred6 = predict(model6, validationData[, c(9, 8, 1, 2, 3, 11, 12)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred6[validationPred6 >= 0.5] = 1
validationPred6[validationPred6 < 0.5] = 0

# Matriz de confusão
cm6 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred6))
print(cm6)


#ACC = (TP + TN) / total
ACC6 = (cm6[1,1] + cm6[2,2]) / sum(cm6)
print(ACC6)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR6 = cm6[2,2] / (cm6[2,2] + cm6[2,1])
print(TPR6)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR6 = cm6[1,1] / (cm6[1,1] + cm6[1,2])
print(TNR6)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm6 = mean(c(TPR6, TNR6))
print(ACCNorm6)

# Tentativa 7 - Quadrático combinado

trainDataQuad2 <- trainData[, c(9, 8, 1, 2, 3, 11, 12)]
trainDataQuad2[paste(names(trainDataQuad2[, 1:6]), ".quad", sep = "")] <- NA

trainDataQuad2[,8:13] <- quadratico(trainDataQuad2[, 1:6])

validationData[paste(names(validationData[, c(9, 8, 1, 2, 3, 11)]), ".quad", sep = "")] <- NA
validationData[,14:19] <- quadratico(validationData[, c(9, 8, 1, 2, 3, 11)])

####### Treinando a regressão logística
formula7 = as.formula("quality ~ pH + density + fixed.acidity + volatile.acidity + citric.acid + alcohol + pH.quad + density.quad + fixed.acidity.quad + volatile.acidity.quad + citric.acid.quad + alcohol.quad")

model7 = glm(formula7, trainDataQuad2, family=binomial(link="logit"))

validationPred7 = predict(model7, validationData[, c(9, 8, 1, 2, 3, 11, 12, 14, 15, 16, 17, 18, 19)], type="response")

# Convertendo para bom ou ruim (classe)
validationPred7[validationPred7 >= 0.5] = 1
validationPred7[validationPred7 < 0.5] = 0

# Matriz de confusão
cm7 = as.matrix(table(Actual = validationData$quality, Predicted = validationPred7))
print(cm7)


#ACC = (TP + TN) / total
ACC7 = (cm7[1,1] + cm7[2,2]) / sum(cm7)
print(ACC7)

#TPR = (TP) / (TP + FN)		ou		TPR = TP / nPos
TPR7 = cm7[2,2] / (cm7[2,2] + cm7[2,1])
print(TPR7)

#TNR = (TN) / (TN + FP)		ou		TNR = TN / nNeg 
TNR7 = cm7[1,1] / (cm7[1,1] + cm7[1,2])
print(TNR7)

#ACC normalizada (leva em conta a quantidade de exemplos
#da classe positiva e da classe negativa)
ACCNorm7 = mean(c(TPR7, TNR7))
print(ACCNorm7)

# Conclusão
vetorACC <- c(ACCNorm, ACCNorm2, ACCNorm3, ACCNorm4, ACCNorm5, ACCNorm6, ACCNorm7)
vetorACCTipos <- c("1", "2", "3", "4", "5", "6", "7")
orderACC <- order(vetorACC, decreasing = T)
vetorACC <- vetorACC[orderACC]
vetorACCTipos <- vetorACCTipos[orderACC]
dataFrameACC <- data.frame(vetorACCTipos, vetorACC)

# Ordenando levels para manter o gráfico conforme ordenação do dataframe
dataFrameACC$vetorACCTipos <- factor(dataFrameACC$vetorACCTipos, levels = dataFrameACC$vetorACCTipos[order(dataFrameACC$vetorACC, decreasing = T)])

ggplot(dataFrameACC, aes(x=vetorACCTipos, y=vetorACC)) + 
  geom_bar(stat="identity", aes(fill = dataFrameACC$vetorACCTipos)) +
  labs(x = "Experimentos", y = "Acurácia", fill = "Legendas")

print(dataFrameACC)

# 8 Random Forest

library(randomForest)

trainData$quality <- as.factor(trainData$quality)

## mtry 3 ou 5
rfModel <- randomForest(formula = quality ~ ., data = trainData, ntree = 15, mtry = 5)

predictionRf = predict(rfModel, validationData)

rfCM = as.matrix(table(Actual = validationData$quality, Predicted = predictionRf))
TPRrf = rfCM[2,2] / (rfCM[2,2] + rfCM[2,1])
TNRrf = rfCM[1,1] / (rfCM[1,1] + rfCM[1,2])
ACCNormrf = mean(c(TPRrf, TNRrf))

ACCNormrf
