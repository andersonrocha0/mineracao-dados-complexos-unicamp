setwd('~/DataScience/Unicamp/INF-0615/2018-04-07')

abalone <- read.csv("abalone.data", header = T, sep = ",")

round(nrow(abalone) * 0.8)
names(abalone)

#1.a Temos 4177 
#1.b Temos 8 variáveis - sex length diameter height whole_weight shucked_weight viscera_weight shell_weight rings

randomSample <- sample(nrow(abalone))
treino <- abalone[randomSample[1:round(nrow(abalone) * 0.8)], ]
teste <- abalone[randomSample[(round(nrow(abalone) * 0.8) + 1): nrow(abalone)], ]

modelo1 <- lm(rings ~ length + diameter + height + whole_weight + shucked_weight + viscera_weight + shell_weight, treino)

predicao1 <- predict(modelo1, teste)

length(predicao1)

sum(abs(teste$rings - predicao1))/nrow(teste)

pontos <- data.frame(x=1:length(predicao1))

# Plotar gráfico para testar isso