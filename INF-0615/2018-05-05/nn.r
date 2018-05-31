#################################
# MDC - Machine Learning		#
# Neural Networks				#
#################################


# load the toy datasets
source("create_dataset.r")

# install and load nn package
install.packages("neuralnet")
library(neuralnet)

#NN only work with formula, so we need to cbind xtrain and ytrain
trainData = cbind(xtrain, ytrain)
colnames(trainData)= c("x1", "x2", "y")

trainData[1:3,]


# train a neuralnet
nnModel = neuralnet(formula="y ~ x1+x2", data=trainData, hidden=c(5,3), linear.output=FALSE) 
#linear.output = TRUE --> regression
#linear.output = FALSE --> classification (apply 'logistic' activation as default)


# General summary
summary(nnModel)

# Plot the network
plot(nnModel)


#### Let's predict our test set
nnCompute = compute(nnModel, xtest)
nnCompute

prediction = nnCompute$net.result

prediction[prediction < 0.5] = -1
prediction[prediction >= 0.5] = 1

CM = as.matrix(table(Actual = ytest, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))






####### non-linear data #########
nlTrainData = cbind(nlxTrain, nlyTrain)
colnames(nlTrainData)= c("x1", "x2", "y")





# train a neuralnet
nnModel = neuralnet(formula="y ~ x1+x2", data=nlTrainData, hidden=c(10,9,8,7,6,5), linear.output=FALSE) 
#linear.output = TRUE --> regression
#linear.output = FALSE --> classification (apply 'logistic' activation as default)

nnCompute = compute(nnModel, nlxTest)
nnCompute

prediction = nnCompute$net.result

prediction[prediction < 0.5] = -1
prediction[prediction >= 0.5] = 1

CM = as.matrix(table(Actual = nlyTest, Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))





######



library(kernlab)
library(neuralnet)

trainData = read.csv("occupancy_dataTrain.txt")
testData = read.csv("occupancy_dataTest.txt")


#### Attribute Information:
#date: string  'year-month-day hour:minute:second'
#Temperature: numeric, in Celsius
#Humidity: numeric, %
#Light: numeric, in Lux
#CO2: numeric in ppm
#Humidity Ratio (derived quantity from temperature and relative humidity)
# 				numeric in kgwater-vapor/kg-air
#Occupancy (class): 0 or 1, 0 for not occupied, 1 for occupied status
summary(trainData)
table(trainData$Occupancy)


# Dropping date, but what if we wanted to use it?
#Idea: create features (weekday/weekend and morning/evening/night)
trainData$date = NULL
testData$date = NULL


# Normalizing data
meanTrainFeatures = colMeans(trainData[,1:5]) #mean of each feature
stdTrainFeatures = apply(trainData[,1:5], 2, sd) #std of each feature

trainData[,1:5] = sweep(trainData[,1:5], 2, meanTrainFeatures, "-")
trainData[,1:5] = sweep(trainData[,1:5], 2, stdTrainFeatures, "/")

testData[,1:5] = sweep(testData[,1:5], 2, meanTrainFeatures, "-")
testData[,1:5] = sweep(testData[,1:5], 2, stdTrainFeatures, "/")



# Change labels to -1 and 1 
trainData$Occupancy[trainData$Occupancy == 0] = -1
testData$Occupancy[testData$Occupancy == 0] = -1


reducedTrainData <- trainData[sample(1:nrow(trainData), 500),]

nnModel = neuralnet(formula="Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio", data=reducedTrainData, hidden=c(10,5), linear.output=FALSE) 
#linear.output = TRUE --> regression
#linear.output = FALSE --> classification (apply 'logistic' activation as default)

nnCompute = compute(nnModel, testData[,1:5])
nnCompute

prediction = nnCompute$net.result

prediction[prediction < 0.5] = -1
prediction[prediction >= 0.5] = 1

CM = as.matrix(table(Actual = testData[, "Occupancy"], Predicted = prediction))
TPR = CM[2,2] / (CM[2,2] + CM[2,1])
TNR = CM[1,1] / (CM[1,1] + CM[1,2])
ACCNorm = mean(c(TPR, TNR))



