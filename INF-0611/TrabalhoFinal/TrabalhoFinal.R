#######################################################################
# Trabalho Final - INF-0611          
# Nome(s): Alexandre Joao Guidin Junior, Anderson Rocha
#######################################################################

#install.packages("TSclust")
#library(TSclust)

#Setando Diretorio
#setwd("/home/alexandre.guidin/Desktop")
#setwd("C:\\Users\\Alexandre-PC\\Desktop")
setwd("/home/anderson/DataScience/Unicamp/INF-0611/TrabalhoFinal")

rm(list = ls())

#Loading CSV
data <- read.csv(file = "cepagri.csv", header = F, col.names = c("date","temp","velocidade","umidade","sensacao"), sep = ";", stringsAsFactors = F)
data$temp <- as.numeric(data$temp)

#Limpando dados
data <- data[!is.na(data$sensacao), ]
data <- data[data$sensacao != 99.9, ]

#Filtrando entre datas (2015-2017)
data$date <- strptime(x=data$date, format = "%d/%m/%Y-%H:%M")
data <- data[(data$date$year >= 115 & data$date$year <= 117),]
data$dateAndHour <- as.POSIXlt(format(data$date, format = "%d/%m/%Y %H"), format = "%d/%m/%Y %H")

# Remoção de dias com leituras incorretas
# 144 é a quantidade de medições que um dia deve ter
# (24 horas * 60 minutos) / 10 minutos
datasComProblemas <- c()
for (d in unique(data$data)) {
  totalDaData <- nrow(data[data$data == d, ])
  if (totalDaData != 144) {
    datasComProblemas <- c(datasComProblemas, d)
  }
}

data <- subset(data, !data %in% datasComProblemas)

#Inserindo o campo dd/MM/yyyy e o campo Hora
data$simpleDate <- as.POSIXlt(format(data$date, format = "%d/%m/%Y"), format = "%d/%m/%Y")
data$simpleDate <- format(data$date, format = "%d/%m/%Y")

#Juntando os dados por hora/dia e ordenando
data <- aggregate(x = data[,2:5], by = list(data$simpleDate), FUN = mean)
names(data)[1] <- "date"
data$date <- as.POSIXct(x = data$date, format = "%d/%m/%Y")
data <- data[order(data$date),]
data <- data[!duplicated(data$date),]

#Loading CSV de query, e colocando os dados de acordo com o dia
query <- read.csv(file = "query.csv", header = F, sep = ",", col.names = c("index", "date"), skip = 1, stringsAsFactors = F)
query$date <- as.POSIXct(x = query$date, format = "%Y-%m-%d")
query <- query[,2]
query <- data[data$date %in% query,]


#query <- query[1:5,]
#data <- data[1:100,]

discretizacao <- function(vec) {
  sapply(vec, function(item) {
    if(item > 0.43) {
      "C"
    }else if(item > 0){
      "B"
    }else {
      "A"
    }
  })
}

reducao <- function(vec){
  result <- c()
  for(i in seq(from=1, to=length(vec), by=2)){
    value <- (vec[i] + vec[i+1]) / 2
    result <- c(result, value)
  }
  result
}

normalizacao <- function(serie){
  ms <- mean(serie)
  ds <- sd(serie)
  serie <- (serie-ms) / ds
  serie
}

sax.mindist <- function(serie1, serie2){
  serie1 <- normalizacao(serie1)
  serie2 <- normalizacao(serie2)
  serie1 <- reducao(serie1)
  serie2 <- reducao(serie2)
  serie1 <- discretizacao(serie1)
  serie2 <- discretizacao(serie2)
  mindist(serie1,serie2)
}

dist <- function(letter1,letter2){
  if(letter1 == "A" & letter2 =="C"){
    0.67
  }else if(letter1 == "C" & letter2 =="A"){
    0.67
  }else {
    0
  }
}

mindist <- function(serie1,serie2) {
  dists <- 0
  for(index in 1:2){
    parcialdist <- dist(serie1[index], serie2[index])
    parcialdist <- parcialdist ^ 2
    dists <- dists + parcialdist
  }
  2 * sqrt(dists)
}

singleDayDist <- function(queryDay, data){
  dataDates <- unique(data$date)
  dists <- c()
  sapply(dataDates, function(dd){
    dat <- na.omit(data[data$date == dd, 2:5])
    if(nrow(dat) > 0){
      dist <- sax.mindist(as.numeric(queryDay), as.numeric(dat))      
      #dist <- diss.MINDIST.SAX(x = as.numeric(queryDay), y= as.numeric(dat), w=1, alpha = 4, plot = F)
      dists <<- c(dists, dist)
    }else{
      dists <<- c(dists, NA)
    }
  })
  dists
}

daysDist <- function(query, data) {
  
  relevancia <- sapply(query$date, function(date){
    qry <- query[query$date == date, 2:5]
    dists <- singleDayDist(qry, data)
    dataAux <- data
    dataAux$distancia <- dists
    dataAux <- dataAux[order(dataAux$distancia),]
    dataAux <- dataAux[1:100,]
    relevante <-((as.Date(dataAux$date) > (as.Date(date) + 7)) | (as.Date(dataAux$date) > (as.Date(date) - 7)))
  })
  
  relevancia <- relevancia[seq(5, 100, by=5),]
}

precisionRecall <- function(relevancia) {
  
  precisions <- c()
  recalls <- c()
  
  sapply(1:100, function(index){
    
    relevantes <- sum(relevancia[,index] == T)
    relevantCount <- 0
    
    itemPrecisionRecall <- sapply(1:20, function(i){
      if(relevancia[i,index] == T){
        relevantCount <<- relevantCount + 1
      }
      
      recall <- (relevantCount / relevantes)
      recall <- ifelse(is.nan(recall), 0, recall)
      precision <- (relevantCount / i) 
      precisions <<- c(precisions, precision)
      recalls <<- c(recalls, recall)
    })
  })
  
  precisions <- rowMeans(matrix(ncol = 100, nrow = 20, data = precisions))
  recalls <- rowMeans(matrix(ncol = 100, nrow = 20, data = recalls))
  data.frame(precision=precisions, recall=recalls)
}

result <- precisionRecall(daysDist(query,data))
print("====== RESULTADO SAX ======")
result



# Estratégia 2

# Calcula dist L1 e L2
calculaDistanciaL1eL2 <- function(vetor, query) {
  tamanhoDoVetor = length(vetor)
  l1 <- sum(abs(sapply(1:tamanhoDoVetor, function(x) sum(query[x])) - sapply(1:tamanhoDoVetor, function(x) sum(vetor[x]))))
  l2 <- sqrt(sum((abs(vetor - query))^2))
  list(l1, l2)
}

# Normalização da query

query$tempRound <- round(query$temp, 2)
query$velocidadeRound <- round(query$velocidade, 2)
query$umidadeRound <- round(query$umidade, 2)
query$sensacaoRound <- round(query$sensacao, 2)

queryMatrix <- data.matrix(query[, 6:9])
queryVector <- apply(queryMatrix, 1, normalizacao)
queryMatrixNormalizada <- matrix(queryVector, ncol = 4, nrow = (length(queryVector) / 4), byrow = TRUE)
queryNormalizada <- as.data.frame(queryMatrixNormalizada)
names(queryNormalizada) <- c("tempNormalizada", "velocidadeNormalizada", "umidadeNormalizada", "sensacaoNormalizada")

query$tempNormalizada <- queryNormalizada$tempNormalizada
query$velocidadeNormalizada <- queryNormalizada$velocidadeNormalizada
query$umidadeNormalizada <- queryNormalizada$umidadeNormalizada
query$sensacaoNormalizada <- queryNormalizada$sensacaoNormalizada


# Para conferir se o resultado está certo verifiquei somente uma linha.
normalizacao(as.numeric(query[1, 2:5]))

# Normalização dos dados

# Arredondamento dos valores para ficar mais fácil de visualizar
data$tempRound <- round(data$temp, 2)
data$velocidadeRound <- round(data$velocidade, 2)
data$umidadeRound <- round(data$umidade, 2)
data$sensacaoRound <- round(data$sensacao, 2)

dataMatrix <- data.matrix(data[, 6:9])
dataVector <- apply(dataMatrix, 1, normalizacao)
dataMatrixNormalizada <- matrix(dataVector, ncol = 4, nrow = (length(dataVector) / 4), byrow = TRUE)
dataNormalizada <- as.data.frame(dataMatrixNormalizada)
names(dataNormalizada) <- c("tempNormalizada", "velocidadeNormalizada", "umidadeNormalizada", "sensacaoNormalizada")

data$tempNormalizada <- dataNormalizada$tempNormalizada
data$velocidadeNormalizada <- dataNormalizada$velocidadeNormalizada
data$umidadeNormalizada <- dataNormalizada$umidadeNormalizada
data$sensacaoNormalizada <- dataNormalizada$sensacaoNormalizada

# Adicionando colunas das distâncias no dataframe query

# Criando uma matriz de 100 pela quantidade de linhas do teste para adicionar ao dataframe de query
matrixLinhasDataFrame <- matrix(0, nrow = nrow(query), ncol = 100)
dataFrameLinhasDataFrame <- as.data.frame(matrixLinhasDataFrame)
distanciasMenu <- paste("DistanciaL2", 1:100, sep=".")
names(dataFrameLinhasDataFrame) <- distanciasMenu

# Adicionando a matriz ao dataframe de query L2
query <- cbind(query, dataFrameLinhasDataFrame)

distanciasMenu <- paste("DistanciaL1", 1:100, sep=".")
names(dataFrameLinhasDataFrame) <- distanciasMenu

# Adicionando a matriz ao dataframe de query L1
query <- cbind(query, dataFrameLinhasDataFrame)

# Pegar a query, comparar com todos os registros do dataframe data e salvar a distância no dataframe.
# Ordenar pelos 100 primeiros.
# Guardar os 100 primeiros dentro de uma lista no dataframe query.

# Número de linhas que o data frame dados tem
numeroDeLinhas <- nrow(data)
numeroDeLinhasQuery <- nrow(query)

# Variável de controle do loop que percorrerá a matriz 
i <- 1

start.time <- Sys.time()
start.time
# primeiro loop para comparar linha a linha com as demais linhas
while (i <= numeroDeLinhasQuery) {
  # Variável de controle do loop comparará linha a linha da matriz
  j <- 1
  
  # Criação de um vetor com a soma das distâncias
  # O vetor foi iniciado com o mesmo tamanho dos dados e com valores iguais a zero
  somasDistanciasL2 <- rep(0, numeroDeLinhas)
  somasDistanciasL1 <- rep(0, numeroDeLinhas)
  
  while(j <= numeroDeLinhas) {
    x <- query[i, 10:13]  
    y <- data[j, 10:13]
    # Neste ponto é somado o valor das distancias para no final cacular a média
    distanciasL1L2 <- calculaDistanciaL1eL2(as.numeric(y), as.numeric(x))
    somasDistanciasL2[j] <- somasDistanciasL2[j] + distanciasL1L2[[2]]
    somasDistanciasL1[j] <- somasDistanciasL1[j] + distanciasL1L2[[1]]
    j <- j + 1
  }
  
  ## Esta parte ira dentro do while
  
  # Adicinando as distâncias no data frame de dados
  data$distanciasL2 <- somasDistanciasL2
  data$distanciasL1 <- somasDistanciasL1
  
  # Pegando os primeiros 100 itens
  primeirosCemItensL2 <- data[order(data$distanciasL2),]
  primeirosCemItensL2 <-primeirosCemItensL2[1:100, ]
  
  primeirosCemItensL1 <- data[order(data$distanciasL1),]
  primeirosCemItensL1 <-primeirosCemItensL1[1:100, ]
  
  query[i,14:113] <- primeirosCemItensL2[, 1]
  query[i,114:213] <- primeirosCemItensL1[, 1]
  
  
  # Testando as datas
  # as.POSIXct(query[1,14], origin = "1970-01-01")
  
  # Somente um log para conseguir acompanhar o status do processamento
  # print(paste(i,"/", numeroDeLinhasQuery))
  i <- i + 1
}
end.time <- Sys.time()
end.time
time.taken <- end.time - start.time
time.taken


# Preparação para o precision e recall

query$dateNumeric <- as.numeric(query$date)

seteDias <- 7 * 24 * 60 * 60

relevantesL2 <- query[, 14:113] >= (query[, 214] - seteDias) & query[, 14:113] <= (query[, 214] + seteDias)
relevantesL2Matrix <- matrix(as.numeric(unlist(relevantesL2)),nrow=nrow(relevantesL2))

relevantesL1 <- query[, 114:213] >= (query[, 214] - seteDias) & query[, 114:213] <= (query[, 214] + seteDias)
relevantesL1Matrix <- matrix(as.numeric(unlist(relevantesL1)),nrow=nrow(relevantesL1))

relevantesL2Passo5 <- relevantesL2[seq(5, 100, by=5),]
relevantesL1Passo5 <- relevantesL1[seq(5, 100, by=5),]

precisionRecallL2 <- precisionRecall(relevantesL2Passo5)
precisionRecallL1 <- precisionRecall(relevantesL1Passo5)

print("====== RESULTADO L2 ======")
precisionRecallL2

print("====== RESULTADO L1 ======")
precisionRecallL1


library(ggplot2)
pL2 <- ggplot(precisionRecallL2, aes(x = recall, y = precision))
pL2 <- pL2 + geom_line()
pL2

pL1 <- ggplot(precisionRecallL1, aes(x = recall, y = precision))
pL1 <- pL1 + geom_line()
pL1

sax <- ggplot(result, aes(x = recall, y = precision))
sax <- sax + geom_line()
sax

pL2ComL1 <- ggplot(precisionRecallL2, aes(x = recall, y = precision)) +
  geom_line (data = precisionRecallL2, aes(x = recall, y = precision, colour = "L2" )) +
  geom_line (data = precisionRecallL1, aes(x = recall, y = precision, colour = "L1" ))
pL2ComL1