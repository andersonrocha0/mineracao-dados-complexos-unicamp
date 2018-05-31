# Exercício 2

# Funções auxiliares
calculaDistanciaL1 <- function(matriz, query) {
  sum(abs(sapply(0:4, function(x) sum(query == x)) - sapply(0:4, function(x) sum(matriz == x))))
}

calculaDistanciaL2 <- function(matriz, query) {
  sqrt(sum((abs(matriz - query))^2))
}

calculaDistanciaL1eL2 <- function(matriz, query) {
  l1 <- calculaDistanciaL1(matriz, query)
  l2 <- calculaDistanciaL2(matriz, query) 
  list(l1, l2)
}
# Final funções

# Criação das matrizes
imgA <- c(0,0,1,2,4,0,0,3,2,4,3,3,1,0,1,3,1,4,2,1,3,4,4,2,2)
imgB <- c(1,1,2,3,4,1,1,4,3,4,4,2,2,1,2,0,0,4,3,2,0,0,4,3,3)
imgC <- c(3,3,1,0,0,3,3,1,0,0,3,2,2,0,0,2,2,2,0,0,1,1,1,0,0)
imgD <- c(4,4,2,0,1,4,4,2,0,1,4,3,3,1,1,3,3,3,0,1,2,2,2,0,0)
matrizImgA <- matrix(imgA, nrow=5, ncol=5, byrow=T)
matrizImgB <- matrix(imgB, nrow=5, ncol=5, byrow=T)
matrizImgC <- matrix(imgC, nrow=5, ncol=5, byrow=T)
matrizImgD <- matrix(imgD, nrow=5, ncol=5, byrow=T)

# Extração dos vetores de características
# Aqui eu agrupo de 0 a 4, que são os valores encontrados nas matrizes e
# faço um count das quantidades encontradas
sapply(0:4, function(x) sum(matrizImgA == x))
# resultado 5 5 5 5 5
sapply(0:4, function(x) sum(matrizImgB == x))
# resultado 4 5 5 5 6
sapply(0:4, function(x) sum(matrizImgC == x))
# resultado 10  5  5  5  0
sapply(0:4, function(x) sum(matrizImgD == x))
# resultado 5 5 5 5 5

# Calculo da distância L1 matriz por matriz
# Matriz A
distMatrizA <- sapply(list(NA, matrizImgB, matrizImgC, matrizImgD), function(x) calculaDistanciaL1(x,matrizImgA))
# Matriz B
distMatrizB <- sapply(list(matrizImgA, NA, matrizImgC, matrizImgD), function(x) calculaDistanciaL1(x,matrizImgB))
# Matriz C
distMatrizC <- sapply(list(matrizImgA, matrizImgB, NA, matrizImgD), function(x) calculaDistanciaL1(x,matrizImgC))
# Matriz D
distMatrizD <- sapply(list(matrizImgA, matrizImgB, matrizImgC, NA), function(x) calculaDistanciaL1(x,matrizImgD))

distMatrizA
# [1] NA  2 10  0
distMatrizB
# [1]  2 NA 12  2
distMatrizC
# [1] 10 12 NA 10
distMatrizD
# [1]  0  2 10 NA

# As distâncias mostram que A é a mais próxima de D. É possível observar isso nos vetores 
# distMatrizA, onde D tem 0 de distância e no vetor distMatrizB, onde A tem 0 de distância.
# As duas imagens mais similares entre si, são A e D