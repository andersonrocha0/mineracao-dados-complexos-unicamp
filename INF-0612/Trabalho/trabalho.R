########################################
# Trabalho - INF-0612          
# Nome(s): Anderson Rocha
########################################


# Importando o arquivo CSV
names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("/home/anderson/DataScience/Unicamp/INF-0612/trabalho/cepagri.csv", header = FALSE, sep = ";", col.names = names)

library(ggplot2)

# Definindo campos que ajudarão nas análises
cepagri$DataHora <- strptime(cepagri$Horario, "%d/%m/%Y-%H:%M")
cepagri$Dia <- cepagri$DataHora$mday
cepagri$Hora <- cepagri$DataHora$hour
cepagri$Data <- as.Date(cepagri$DataHora)
cepagri$Ano <- cepagri$DataHora$year
cepagri$Mes <- cepagri$DataHora$mon

# Conforme enunciado, analisar apenas os dados compreendidos no intervalo de 01/01/2015 a 31/12/2017
dataInicial <- strptime("01/01/2015-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinal <- strptime("31/12/2017-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltrado <- cepagri[cepagri$DataHora >= dataInicial & cepagri$DataHora <= dataFinal,]

# Há um erro na coluna temperatura
summary(cepagriFiltrado)
# Remoção do erro
cepagriFiltrado <- cepagriFiltrado[trimws(cepagriFiltrado$Temperatura) != "[ERRO]", ]

# Convertendo temperatura para numeric
cepagriFiltrado$Temperatura <- as.numeric(as.character(cepagriFiltrado$Temperatura))

# Ainda há um erro na máxima da sensação, com 99,90
summary(cepagriFiltrado)

cepagriFiltrado <- cepagriFiltrado[cepagriFiltrado$Sensacao != 99.9, ]

# Remoção de dias com leituras incorretas
# 144 é a quantidade de medições que um dia deve ter
# (24 horas * 60 minutos) / 10 minutos
datasComProblemas <- c()
for (data in unique(cepagriFiltrado$Data)) {
  totalDaData <- nrow(cepagriFiltrado[cepagriFiltrado$Data == data, ])
  if (totalDaData != 144) {
    datasComProblemas <- c(datasComProblemas, data)
    #print(paste("Quantidade de medições do dia n.", as.Date(data, origin = "1970-01-01"), '(', data, ')', '-', totalDaData))
  }
}

cepagriFiltrado <- subset(cepagriFiltrado, !Data %in% datasComProblemas)

summary(cepagriFiltrado)

# Aparentemente os dados estão com números melhores. Segue abaixo as análises:

# Análise 1 - Inverno com a média de temperatura mais baixa
# No Hemisfério Sul, o inverno também é chamado de inverno austral. 
# Ele começa no dia 21 de junho e termina no dia 23 de setembro. É o caso do Brasil.

inicioInverno <- "21/06"
fimInverno <- "23/09"

menorTemperatura <- 99
anoMenorMediaTemperatura <- 0

for (i in unique(cepagriFiltrado$Ano)) {
  inicioInverno2 <- as.Date(paste(inicioInverno, "/", i + 1900, sep = ""), "%d/%m/%Y")
  fimInverno2 <- as.Date(paste(fimInverno, "/", i + 1900, sep = ""), "%d/%m/%Y")
  cepagriInverno <- cepagriFiltrado[cepagriFiltrado$Data >= inicioInverno2 & cepagriFiltrado$Data <= fimInverno2, ]
  mt <- mean(cepagriInverno$Temperatura)
  if (mt < menorTemperatura) {
    menorTemperatura <- mt
    anoMenorMediaTemperatura <- i + 1900
  }
}

print(paste("O ano com menor temperatura média no inverno foi:", anoMenorMediaTemperatura, "com a temperatura de:", menorTemperatura))

# Análise 2 - Data da máxima do vento

print(cepagriFiltrado[cepagriFiltrado$Vento == max(cepagriFiltrado$Vento), 9])

# Análise 3 - Menor sensação térmica do ano de 2017

cepagriFiltrado2017 <- cepagriFiltrado[cepagriFiltrado$Ano == 117, ]
print(min(cepagriFiltrado2017$Sensacao))


# Análise 4 - Mediana da umidade no mês de março de 2015

dataInicialMarco2015 <- strptime("01/03/2015-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinalMarco2015 <- strptime("31/03/2015-23:59:59", "%d/%m/%Y-%H:%M:%S")

cepagriFiltradoMarco2015 <- cepagriFiltrado[cepagriFiltrado$DataHora >= dataInicialMarco2015 & cepagriFiltrado$DataHora <= dataFinalMarco2015,]
print(median(cepagriFiltradoMarco2015$Umidade))


# Tabelas

# Tabela 1 - A média de temperatura, vento, umidade e sensação por dia de março de 2015
tabela1 <- aggregate(cepagriFiltradoMarco2015[, c(2, 3, 4, 5, 9)], by = list(cepagriFiltradoMarco2015$Dia), mean)
tabela1[, 2:5] <- round(tabela1[, 2:5], 2)
names(tabela1)[1] <- "Dia"
View(tabela1)
#write.csv(tabela1, file = "~/tabela1.csv")

# Tabela 2 - A temperatura máxima por mês e por ano
tabela2 <- aggregate(cepagriFiltrado[, c(2)], by = list(cepagriFiltrado$Mes, cepagriFiltrado$Ano), max)
names(tabela2)[1:2] <- c("Mes", "Ano")
tabela2$Mes <- tabela2$Mes + 1
tabela2$Ano <- tabela2$Ano + 1900
View(tabela2)
#write.csv(tabela2, file = "~/tabela2.csv")

# Gráficos

# Gráfico 1 - Média temperatura vs média sensação março de 2015
tabela1Grafico <- ggplot(tabela1, aes(x = Dia, y = Temperatura)) +
  geom_line (data = tabela1, aes(x = Dia, y = Temperatura, colour = "Temperatura" )) +
  geom_line (data = tabela1, aes(x = Dia, y = Sensacao, colour = "Sensacao" ))

# Gráfico 2 - Umidade relativa dos 10 primeiros dias de 2017
dataInicialPrimeiros2017 <- strptime("01/01/2017-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinalPrimeiros2017 <- strptime("10/01/2017-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltradoPrimeiros2017 <- cepagriFiltrado[cepagriFiltrado$DataHora >= dataInicialPrimeiros2017 & cepagriFiltrado$DataHora <= dataFinalPrimeiros2017,]
p <- ggplot(cepagriFiltradoPrimeiros2017, aes(x = DataHora, y = Umidade))
p <- p + geom_line()
p

# Gráfico 3 - Sensação térmica da segunda quinzena de dezembro de 2015
dataInicialDezembro2015 <- strptime("16/12/2015-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinalDezembro2015 <- strptime("31/12/2015-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltradoDezembro2015 <- cepagriFiltrado[cepagriFiltrado$DataHora >= dataInicialDezembro2015 & cepagriFiltrado$DataHora <= dataFinalDezembro2015,]
p <- ggplot(cepagriFiltradoDezembro2015, aes(x = Sensacao))
p <- p + geom_histogram(bins = 30)
p

# Gráfico 4 - Vento vs temperatura do dia 25 de dezembro de 2015
dataInicialNatal2015 <- strptime("25/12/2015-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinalNatal2015 <- strptime("25/12/2015-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltradoNatal2015 <- cepagriFiltrado[cepagriFiltrado$DataHora >= dataInicialNatal2015 & cepagriFiltrado$DataHora <= dataFinalNatal2015,]
p <- ggplot(cepagriFiltradoNatal2015, aes(x = DataHora))
p <- p + geom_point (aes(y = Temperatura, colour = "Temperatura"), alpha = 0.5)
p <- p + geom_line (aes(y = Vento, colour = "Vento"))
p

# Gráfico 5 - Temperatura dos últimos sete dias do mês de março de 2016
dataInicialMarco2016 <- strptime("25/03/2016-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinalMarco2016 <- strptime("31/03/2016-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltradoMarco2016 <- cepagriFiltrado[cepagriFiltrado$DataHora >= dataInicialMarco2016 & cepagriFiltrado$DataHora <= dataFinalMarco2016,]
p <- ggplot(cepagriFiltradoMarco2016, aes(x = Dia, y = Temperatura, group = Dia))
p <- p + geom_boxplot()
p