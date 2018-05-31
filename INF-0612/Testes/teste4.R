########################################
# Teste 4 - INF-0612          
# Nome(s): Anderson Rocha
########################################

names <- c("Horario", "Temperatura", "Vento", "Umidade", "Sensacao")
cepagri <- read.csv("/home/anderson/DataScience/Unicamp/INF-0612/cepagri.csv", header = FALSE, sep = ";", col.names = names)

library(ggplot2)

cepagri$DataHora <- strptime(cepagri$Horario, "%d/%m/%Y-%H:%M")
cepagri$Dia <- cepagri$DataHora$mday
cepagri$Hora <- cepagri$DataHora$hour

## 1 - Umidade Relativa do Ar
dataInicial <- strptime("01/01/2018-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinal <- strptime("10/01/2018-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltrado <- cepagri[cepagri$DataHora >= dataInicial & cepagri$DataHora <= dataFinal,]
p <- ggplot(cepagriFiltrado, aes(x = DataHora, y = Umidade))
p <- p + geom_line()
p

## 2 - Sensacao Termica da Segunda Quinzena do Mes
dataInicial <- strptime("15/01/2018-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinal <- strptime("31/01/2018-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltrado <- cepagri[cepagri$DataHora >= dataInicial & cepagri$DataHora <= dataFinal,]
p <- ggplot(cepagriFiltrado, aes(x = Sensacao))
p <- p + geom_histogram(bins = 30)
p

## 3 - Temperatura dos Ultimos Sete Dias do Mes
dataInicial <- strptime("25/01/2018-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinal <- strptime("31/01/2018-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltrado <- cepagri[cepagri$DataHora >= dataInicial & cepagri$DataHora <= dataFinal,]
p <- ggplot(cepagriFiltrado, aes(x = Dia, y = Temperatura, group = Dia))
p <- p + geom_boxplot()
p

## 4 - Ventos do Primeiro Dia do Mes
dataInicial <- strptime("01/01/2018-00:00:00", "%d/%m/%Y-%H:%M:%S")
dataFinal <- strptime("01/01/2018-23:59:59", "%d/%m/%Y-%H:%M:%S")
cepagriFiltrado <- cepagri[cepagri$DataHora >= dataInicial & cepagri$DataHora <= dataFinal,]
cepagriFiltrado <- cepagriFiltrado[order(cepagriFiltrado$Hora, cepagriFiltrado$Vento, decreasing = T), ]
cepagriFiltrado <- cepagriFiltrado[!duplicated(cepagriFiltrado[,'Hora']),]
p <- ggplot(cepagriFiltrado, aes(x = Hora, y = Vento))
p <- p + geom_point() + geom_smooth()
p

# Bonus
# 144 e a quantidade de medicoes que um dia deve ter
# (24 horas * 60 minutos) / 10 minutos
for (dia in unique(cepagri$Dia)) {
  totalDoDia <- nrow(cepagri[cepagri$Dia == dia, ])
  if (totalDoDia != 144) {
    print(paste("Quantidade de medições do dia n.", dia, '-', totalDoDia))
  }
}

# Apos essa consulta podemos ver que nos dias 13 e 14 
# nao foram realizadas todas as medições do dia