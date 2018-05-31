#############################
# MDC - Machine Learning	#
# Logistic Regression		#
# Breast Cancer				#
#############################

rm(list = ls())

setwd('~/DataScience/Unicamp/INF-0615/2018-04-14')

# Load the data
trainData = read.csv("breastCancer_train.data", header=TRUE) 
testData = read.csv("breastCancer_test.data", header=TRUE) 


############ Inspect the Data ############
print(summary(trainData))


########### Removing 'id' collum
trainData[,"id"] = NULL
testData[,"id"] = NULL


# error because bare.nuclei looks like a factor
print(cor(trainData))

########### Transform to numeric
trainData$bare.nuclei = as.numeric(trainData$bare.nuclei)
testData$bare.nuclei = as.numeric(testData$bare.nuclei)

trainData = trainData[trainData$bare.nuclei != 11,]
testData = testData[testData$bare.nuclei != 11,]


print(cor(trainData))



####### Normalizing the data
meanTrainFeatures = colMeans(trainData[,1:9]) #mean of each feature
stdTrainFeatures = apply(trainData[,1:9], 2, sd) #std of each feature

print(meanTrainFeatures)
print(stdTrainFeatures)

trainData[,1:9] = sweep(trainData[,1:9], 2, meanTrainFeatures, "-")
trainData[,1:11] = sweep(trainData[,1:9], 2, stdTrainFeatures, "/")

testData[,1:9] = sweep(testData[,1:9], 2, meanTrainFeatures, "-")
testData[,1:9] = sweep(testData[,1:9], 2, stdTrainFeatures, "/")

print(trainData[1:5,])
print(summary(trainData))


####### Training Logistic Regression
formula = as.formula("class ~ clump.thickness + unif.cell.size + unif.cell.shape + 
                     marginal.adhesion + epithelial.cell.size + bare.nuclei +       
                     bland.chromatin + normal.nucleoli + mitoses")

#### error!!! class must be a 0 <= class <= 1
model = glm(formula, trainData, family=binomial(link="logit"))

print(trainData$class)

# Change class = 2 to class = 0
#and class = 4 to class = 1
trainData[trainData$class == 2, "class"] = 0
trainData[trainData$class == 4, "class"] = 1

print(trainData$class)

testData[testData$class == 2, "class"] = 0
testData[testData$class == 4, "class"] = 1


#trainData$class = factor(trainData$class)
#testData$class = factor(testData$class, labels = levels(trainData$class))



# Let's try again
logRegModel = glm(formula, trainData, family=binomial(link="logit"))
print(summary(logRegModel))

# probabilities of being the class 0
testPred = predict(logRegModel, testData[,1:9], type="response")
print(testPred)

#converting to class
testPred[testPred >= 0.5] = 1
testPred[testPred < 0.5] = 0



##### Let's see how well we did

#confusion matrix
cm = as.matrix(table(Actual = testData$class, Predicted = testPred))
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