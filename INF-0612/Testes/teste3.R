########################################
# Teste 3 - INF-0612          
# Nome(s): Anderson Rocha
########################################


## 1 - Maximo Divisor Comum
gcd2 <- function(x, y) {
  if (y == 0) {
    return(x)
  } else {
    return(gcd2(y, x %% y))
  }
}

gcd <- function(...) { 
  vector <- c(...)
  if (length(vector) == 2) {
    gcd2(vector[1], vector[2])
  } else {
    gcd(vector[1], gcd(vector[2:length(vector)]))
  }
}

## 2 - Moda da Idade da Turma
count <- function(vector, element) {
  count <- 0
  for (i in vector) {
    if (i == element) {
      count <- count + 1
    }
  }
  return(count)
}

mode <- function(vector = c()) {
  if (length(vector) == 0) {
    return(NA)
  }
  vetor_com_unicos <- unique(vector)
  moda <- vector[1]
  maiorQuantidade <- 0
  for (valor in vetor_com_unicos) {
    quantidade <- count(vector, valor)
    if (quantidade > maiorQuantidade) {
      maiorQuantidade <- quantidade
      moda <- c(valor)
    } else if (quantidade == maiorQuantidade) {
      moda <- c(moda, valor)
    }
  }
  moda
}

## 3 - Binario para Decimal
binToDec <- function(...) {
  listaDeArrayDeBinarios <- list(...)
  decimais <- c()
  for (binarios in listaDeArrayDeBinarios) {
    expoente <- length(binarios) - 1
    decimal <- 0
    for (binario in binarios) {
      decimal <- decimal + (binario * (2^expoente))
      expoente <- expoente - 1
    }
    decimais <- c(decimais, decimal)
  }
  decimais
}

## 4 - Ocorrencia de palavras
wordCount <- function(word, text) {
  palavraCaixaBaixa <- tolower(word)
  textoCaixaBaixa <- gsub("[?.,!]", "", tolower(text))
  
  vectorDePalavras <- strsplit(textoCaixaBaixa, split = " ")[[1]]
  return(count(vectorDePalavras, palavraCaixaBaixa))
}