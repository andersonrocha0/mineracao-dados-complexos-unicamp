transacoes <- read.transactions(file.choose(), format = "basket", sep = ",")

inspect(transacoes)

regras <- apriori(transacoes, parameter = list(supp = 0.5, conf = 0.5))

inspect(regras)

regras <- eclat(transacoes, parameter = list(supp = 0.1, maxlen = 15))

inspect(regras)

regras <- eclat(transacoes, parameter = list(supp = 0.5, maxlen = 15))