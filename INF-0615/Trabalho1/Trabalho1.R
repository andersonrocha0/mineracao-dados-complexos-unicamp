########################################
# Trabalho - INF-0615          
# Nome(s): Anderson Rocha
########################################

setwd("/home/anderson/DataScience/Unicamp/INF-0615/Trabalho1/")

rm(list = ls())

trainData <- read.csv(file = "housePricing_trainSet.csv", header = T, sep = ",")
validationData <- read.csv(file = "housePricing_valSet.csv", header = T, sep = ",")
testData <- read.csv(file = "housePricing_testSet.csv", header = T, sep = ",")

# Função z norm
normalizacao <- function(serie){
  ms <- mean(serie)
  print(ms)
  ds <- sd(serie)
  print(ds)
  serie <- (serie-ms) / ds
  serie
}

# Quantidade de linhas do data set de treino
nrow(trainData)

# 12384

# Analises

summary(trainData)
cor(trainData[ , 1:9], use = "na.or.complete")

# Preenchendo o campo total de quartos com informção NA pela média da quantidade de quartos.
mediaNosQuartos <- function(dataframe) {
  dataframe$total_bedrooms[is.na(dataframe$total_bedrooms)] <- mean(dataframe$total_bedrooms, na.rm = T)  
  dataframe
}

trainData <- mediaNosQuartos(trainData) 

## TODO nunca olhar no conjunto de testes
validationData <- mediaNosQuartos(validationData)
testData <- mediaNosQuartos(testData)

# Transformando features discretas em contínuas
# Verificando o qual tipo de imóvel é mais caro

pesosDiscretasEmContinuas <- function(dataframe) {
  proximidadesOceano <- c("INLAND", "<1H OCEAN", "NEAR BAY", "NEAR OCEAN", "ISLAND")  
  pesosProximidadesOceano <- rep(0, 5)
  proximidadesVsPeso <- data.frame(proximidadesOceano, pesosProximidadesOceano)
  
  # Verificando valor mediano da casa através da quantidade de quartos/bedrooms
  # O menor intevalo de quartos que tem todas as distâncias entre o oceano, são das casas que tem
  # mais de 213 quartos e menos de 265
  for (i in 1:length(proximidadesOceano)) {
    proximidadesVsPeso[i, 2] <- mean(dataframe[dataframe$total_bedrooms > 213 & dataframe$total_bedrooms < 265 & dataframe$ocean_proximity == proximidadesOceano[i], 9], na.rm = T)
  }
  
  # Ordenando as medianas para gerar um peso
  proximidadesVsPeso <- proximidadesVsPeso[order(proximidadesVsPeso[, 2]),]
 
  # Gerando gráficos para se a ordem faz sentido
  #library(ggplot2)
  
  #ggplot(dataframe, aes(x = longitude, y = latitude)) + 
  #geom_point(aes(colour = ocean_proximity), alpha = 0.5)
  
  proximidadesVsPeso
}

# Gerando um peso para cada valor mediano.
# Estou gerando o peso, pegando o valor mediano daqueles imóves e dividindo por 100.000
# Abastecendo o dataframe com as features agora contínuas

proximidadesVsPeso <- pesosDiscretasEmContinuas(trainData)

trainData$ocean_proximity_weight <- 0
for (i in 1:nrow(proximidadesVsPeso)) {
  trainData[trainData$ocean_proximity == proximidadesVsPeso[i, 1], 11] <- (proximidadesVsPeso[i, 2] / 100000)
}

levels(validationData$ocean_proximity) <- levels(trainData$ocean_proximity)
validationData$ocean_proximity_weight <- 0
for (i in 1:nrow(proximidadesVsPeso)) {
  validationData[validationData$ocean_proximity == proximidadesVsPeso[i, 1], 11] <- (proximidadesVsPeso[i, 2] / 100000)
}

levels(testData$ocean_proximity) <- levels(trainData$ocean_proximity)
testData$ocean_proximity_weight <- 0
for (i in 1:nrow(proximidadesVsPeso)) {
  testData[testData$ocean_proximity == proximidadesVsPeso[i, 1], 11] <- (proximidadesVsPeso[i, 2] / 100000)
}

normalizaDados <- function(dataframe) {
  # Criando valores normalizados no data frame
  dataframe$longitude_norm <- normalizacao(dataframe$longitude)
  dataframe$latitude_norm <- normalizacao(dataframe$latitude)
  dataframe$housing_median_age_norm <- normalizacao(dataframe$housing_median_age)
  dataframe$total_rooms_norm <- normalizacao(dataframe$total_rooms)
  dataframe$total_bedrooms_norm <- normalizacao(dataframe$total_bedrooms)
  dataframe$population_norm <- normalizacao(dataframe$population)
  dataframe$households_norm <- normalizacao(dataframe$households)
  dataframe$median_income_norm <- normalizacao(dataframe$median_income)
  dataframe$median_house_value_norm <- normalizacao(dataframe$median_house_value)  
  dataframe
}

trainData <- normalizaDados(trainData)
validationData <- normalizaDados(validationData)
testData <- normalizaDados(testData)

# Modelo e predição com o preço normalizado

model1 <- lm(
  median_house_value_norm ~ 
    longitude_norm + 
    latitude_norm + 
    housing_median_age_norm + 
    total_rooms_norm + 
    total_bedrooms_norm + 
    population_norm + 
    households_norm +
    median_income_norm +
    ocean_proximity_weight
  , trainData)


prediction1 <- predict(model1, validationData)

maePrediction1 <- sum(abs(validationData$median_house_value_norm - prediction1))/nrow(validationData)


# Modelo e predição com o preço normal

model2 <- lm(
  median_house_value ~ 
    longitude_norm + 
    latitude_norm + 
    housing_median_age_norm + 
    total_rooms_norm + 
    total_bedrooms_norm + 
    population_norm + 
    households_norm +
    median_income_norm +
    ocean_proximity_weight
  , trainData)


prediction2 <- predict(model2, validationData)

maePrediction2 <- sum(abs(validationData$median_house_value - prediction2))/nrow(validationData)

## TestData

testPrediction <- predict(model2, testData)

maeTestPrediction <- sum(abs(testData$median_house_value - testPrediction))/nrow(testData)